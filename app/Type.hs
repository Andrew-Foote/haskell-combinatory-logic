module Type where

import Data.Char
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- Type variables

{- We have two distinct variable "namespaces": there are named variables, whose name is a string,
and fresh variables, whose name is an integer. This allows us to quickly generate fresh variables
using a counter which increments as each new fresh variable is generating, without worrying about
whether the fresh variable's name clashes with that of an existing variable. The work to assign
readable names to fresh variables is deferred to when we pretty-print the type.
-}
data Var = NamedVar String | FreshVar Int
    deriving (Eq, Show)

instance Hashable Var where
    hashWithSalt salt (NamedVar s) = hashWithSalt salt ((0, s) :: (Int, String))
    hashWithSalt salt (FreshVar i) = hashWithSalt salt ((1, i) :: (Int, Int))

-- | Get the name of a named variable; fails if the variable is fresh.
varName :: Var -> Maybe String
varName (NamedVar s) = Just s
varName (FreshVar _) = Nothing

{-| Convert an index (non-negative integer) to a name for a type variable. The mapping is as
follows:

  0  -> "a",  1  -> "b",  2  -> "c",  ..., 25 -> "z",
  26 -> "a1", 27 -> "b1", 28 -> "c1", ..., 51 -> "z1",
  52 -> "a2", 53 -> "b2", b4 -> "c2", ..., 77 -> "z2",
  ...
-}
varNameFromIndex :: Int -> String
varNameFromIndex i = case divMod i 26 of
    (q, r) -> chr (ord 'a' + r) : (if q == 0 then "" else show q)

infixr 0 :->

-- Types

data Type = VT Var | Type :-> Type
    deriving (Eq, Show)

vars :: Type -> [Var]
vars (VT a) = [a]
vars (a :-> b) = vars a ++ vars b

{-| Assign names to each of the fresh type variables in a type. The fresh type variables are
scanned in leftmost, outermost order, and assigned indices which are consistent with this order.
The varNameFromIndex function is then used to derive a name from each index. If necessary, indices
will be skipped over to avoid clashes with existing named variables in the type.
-}
nameFreshVars :: Type -> Type
nameFreshVars a = let names = Set.fromList $ mapMaybe varName $ vars a in
    case nameFreshVarsIter a names Map.empty 0 of
        (a1, _, _, _) -> a1
    where
        nameFreshVarsIter :: Type -> Set.HashSet String -> Map.HashMap Int String -> Int
                                -> (Type, Set.HashSet String, Map.HashMap Int String, Int)

        nameFreshVarsIter (VT (NamedVar name)) names freshNames i = (VT $ NamedVar name, names, freshNames, i)

        nameFreshVarsIter (VT (FreshVar i)) names freshNames j = case Map.lookup i freshNames of
            Just name -> (VT $ NamedVar name, names, freshNames, j)
            Nothing -> let name = varNameFromIndex j in
                if Set.member name names
                    then nameFreshVarsIter (VT $ FreshVar i) names freshNames $ j + 1
                    else (VT $ NamedVar name, names, Map.insert i name freshNames, j + 1)

        nameFreshVarsIter (b :-> c) names freshNames j = case nameFreshVarsIter b names freshNames j of
            (b1, names1, freshNames1, j1) -> case nameFreshVarsIter c names1 freshNames1 j1 of
                (c1, names2, freshNames2, j2) -> (b1 :-> c1, names2, freshNames2, j2)

{-| Returns a string representation of a type suitable for pretty-printing. All fresh variables in
the type are assigned names beforehand, and the function type constructor is represented by the
infix "->".
-}
prettyType :: Type -> String
prettyType a = prettyType1 $ nameFreshVars a
    where
        prettyType1 (VT (NamedVar name)) = name
        prettyType1 (VT (FreshVar _)) = error "a fresh var remains!"
        prettyType1 (b :-> c) = prettyArgType b ++ " -> " ++ prettyType c
            where
                prettyArgType d = case d of
                    (_ :-> _) -> "(" ++ prettyType1 d ++ ")"
                    _ -> prettyType1 d

-- Type environments and unification

{-| A type environment, i.e. an environment mapping type variables to types.

The type a type variable is mapped to may contain type variables which are themselves mapped to
types by the environment, so that fully expanding a type may require multiple environment lookups.
This is a performance trade-off: in exchange, when doing unification, if we find that a type
variable a needs to be unified with a type A, we can simply insert the mapping a -> A into the
environment, without needing to also eliminate a from the values of all existing mappings. -}
newtype Env = Env { unEnv :: Map.HashMap Var Type }
    deriving (Eq, Show)

-- | The "ground", i.e. empty, type environment.
groundEnv :: Env
groundEnv = Env Map.empty

-- | Check whether a type variable is "bound", i.e. is mapped to a type, in a type environment.
bound :: Var -> Env -> Bool
bound a env = Map.member a $ unEnv env

{-| Retrieve the type a type variable is "bound" to (mapped to) in a type environment; fails if the
variable is not bound in the environment.
-}
boundValue :: Var -> Env -> Maybe Type
boundValue a env = Map.lookup a $ unEnv env

{-| Bind a type variable to a type (i.e. insert a mapping from the variable to the type) in a type
environment.
-}
bind :: Var -> Type -> Env -> Env
bind a b env = Env $ Map.insert a b $ unEnv env

{-| Fully expand a type in a type environment, so that it will no longer contain any type variables
bound in the environment.
-}
expand :: Type -> Env -> Type

expand (VT a) env = case boundValue a env of
    Just b -> expand b env
    Nothing -> VT a

expand (a :-> b) env = expand a env :-> expand b env

{-| Check whether a type variable occurs in a type, using the given type environment to expand the
type if necessary. It is assumed that the variable is not bound in the environment. 
-}
occurs :: Var -> Type -> Env -> Bool

occurs a (VT b) env = (a == b) || case boundValue b env of
    Just c -> occurs a c env
    Nothing -> False

occurs a (b :-> c) env = occurs a b env || occurs a c env

{-| Return a type environment, extending the one given, which unifies the two given types, i.e. is
such that when the types are fully expanded in the new environment, they are equal. Furthermore,
the unification is as "general" as possible, in the sense that any substitution which unifies the
two types can be written as s . e, where s is another substitution and e is the substitution
corresponding to the type environment (i.e. the function flip expand env).
-}
unify :: Type -> Type -> Env -> Maybe Env

unify (VT a) (VT b) env = if a == b
    then Just env
    else case boundValue a env of
        Just a1 -> unify a1 (VT b) env
        Nothing -> case boundValue b env of
            Just b1 -> unify (VT a) b1 env
            Nothing -> Just $ bind a (VT b) env

unify (VT a) b env = case boundValue a env of
    Just a1 -> unify a1 b env
    Nothing -> if occurs a b env
        then Nothing
        else Just $ bind a b env

unify a (VT b) env = unify (VT b) a env

unify (a :-> b) (c :-> d) env = unify a c env >>= unify b d

-- Fresh type variables

-- | A supply to draw new fresh variables from. This is really just a counter.
newtype FreshVarSupply = FreshVarSupply { unFreshVarSupply :: Int }

-- | A fresh variable supply which no fresh variables have been drawn from yet.
initialFreshVarSupply :: FreshVarSupply
initialFreshVarSupply = FreshVarSupply 0

{- Draw a fresh variable from the given supply, returning the variable and the new supply which no
longer contains the drawn variable. -}
freshVar :: FreshVarSupply -> (Var, FreshVarSupply)
freshVar supply = (FreshVar i, FreshVarSupply $ i + 1)
    where i = unFreshVarSupply supply