module Main where

data Term = S | K | App Term Term deriving (Eq, Show)

reduce1 :: Term -> Maybe Term
reduce1 t = case t of
    App (App (App S u) v) w -> Just $ App (App u w) (App v w)
    App (App K u) _ -> Just u
    App u v ->
        case (reduce1 u) of
            Just w -> Just (App w v)
            Nothing -> case (reduce1 v) of
                Just w -> Just (App u w)
                Nothing -> Nothing
    _ -> Nothing

reduce :: Term -> Term
reduce t = maybe t reduce (reduce1 t)

prettyTerm :: Term -> String
prettyTerm t = case t of
    S -> "S"
    K -> "K"
    App u v -> prettyTerm u ++ recurse v
  where
    recurse v = case v of
        App _ _ -> "(" ++ prettyTerm v ++ ")"
        _ -> prettyTerm v

b :: Term
b = App (App S (App K S)) K

main :: IO ()
main = putStrLn $ prettyTerm $ reduce $ App (App (App b K) S) K
