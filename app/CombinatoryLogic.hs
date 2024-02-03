module CombinatoryLogic where

import Type (Type, Type( (:->) ) )
import qualified Type

-- Abstract syntax

data Term = S | K | App Term Term deriving (Eq, Show)

-- Operational semantics

{-| Apply the first applicable reduction rule once to a term. This may fail if there are
no applicable reducton rules. Heads are reduced before arguments, so the reduction is
normal-order.
-}
reduce1 :: Term -> Maybe Term
reduce1 t = case t of
    App (App (App S u) v) w -> Just $ App (App u w) (App v w)
    App (App K u) _ -> Just u
    App u v ->
        case reduce1 u of
            Just w -> Just (App w v)
            Nothing -> case reduce1 v of
                Just w -> Just (App u w)
                Nothing -> Nothing
    _ -> Nothing

{-| Repeatedly apply the first applicable reduction rule to a term until it is no longer
reducible. Heads are reduced before arguments, so the reduction is normal-order. Some
terms never become reducible, so a call to this function may not terminate.
-}
reduce :: Term -> Term
reduce t = maybe t reduce (reduce1 t)

-- Type inference

inferType :: Term -> Type.Env -> Type.FreshVarSupply -> Maybe (Type.Type, Type.Env, Type.FreshVarSupply)

inferType S env supply = case Type.freshVar supply of
    (a, supply1) -> case Type.freshVar supply1 of
        (b, supply2) -> case Type.freshVar supply2 of
            (c, supply3) -> Just (
                (Type.VT a :-> Type.VT b :-> Type.VT c) :-> (Type.VT a :-> Type.VT b) :-> Type.VT a :-> Type.VT c,
                env, supply3)

inferType K env supply = case Type.freshVar supply of
    (a, supply1) -> case Type.freshVar supply1 of
        (b, supply2) -> Just (Type.VT a :-> Type.VT b :-> Type.VT a, env, supply2)

inferType (App t u) env supply = case inferType t env supply of
    Just (a, env1, supply1) -> case inferType u env1 supply1 of
        Just (b, env2, supply2) -> case Type.freshVar supply2 of
            (c, supply3) -> case Type.unify a (b :-> Type.VT c) env2 of
                Just env3 -> Just (Type.VT c, env3, supply3)
                Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

principalType :: Term -> Maybe Type.Type
principalType t = case inferType t Type.groundEnv Type.initialFreshVarSupply of
    Just (a, env, _) -> Just $ Type.expand a env
    Nothing -> Nothing

-- Concrete syntax

-- |Convert a term to a string representation using a minimal amount of parentheses.
prettyTerm :: Term -> String
prettyTerm t = case t of
    S -> "S"
    K -> "K"
    App u v -> prettyTerm u ++ recurse v
  where
    recurse v = case v of
        App _ _ -> "(" ++ prettyTerm v ++ ")"
        _ -> prettyTerm v

-- it's my first haskell program, so let's keep it simple and not get into parser
-- combinators already

-- |Parse a term from a string.
parseTerm :: String -> Maybe Term
parseTerm s = case termParser s of
    Just (t, []) -> Just t
    _ -> Nothing
    
termParser :: String -> Maybe (Term, String)
termParser s = case funParser s of
    Just (t, s1) -> case argsParser s1 of
        Just (us, s2) -> Just (foldl App t us, s2)
        Nothing -> Nothing
    Nothing -> Nothing

funParser :: String -> Maybe (Term, String)
funParser s = case s of
    'S' : s1 -> Just (S, s1)
    'K' : s1 -> Just (K, s1)
    '(' : s1 -> case termParser s1 of
        Just (t, ')' : s2) -> Just (t, s2)
        _ -> Nothing
    _ -> Nothing

argsParser :: String -> Maybe ([Term], String)
argsParser s = case funParser s of
    Just (t, s1) -> case argsParser s1 of
        Just (us, s2) -> Just (t : us, s2)
        Nothing -> Nothing
    Nothing -> Just ([], s)
