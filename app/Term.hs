module Term where

import Control.Applicative
import Type (Type, Type( (:->) ) )
import qualified Type

import Parser

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

-- |Parse a term from a string.
parseTerm :: String -> Maybe Term
parseTerm s = case unParser termParser s of
    Just (t, []) -> Just t
    _ -> Nothing

termParser :: Parser Term
termParser = liftA2 (foldl App) funParser argsParser

funParser :: Parser Term
funParser =
    charParser (\case { 'S' -> Just S ; 'K' -> Just K ; _ -> Nothing })
    <|>
    liftA3 (\_ x _ -> x) (consumeChar '(') termParser (consumeChar ')')

argsParser :: Parser [Term]
argsParser = liftA2 (:) funParser argsParser <|> pure []
    