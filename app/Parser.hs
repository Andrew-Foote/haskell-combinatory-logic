module Parser where

import Control.Applicative

type Parser a = String -> Maybe (a, String)

abort :: Parser a
abort = const empty

emit :: a -> Parser a
emit v s = pure (v, s)

consume :: (Char -> Maybe a) -> Parser a
consume f [] = empty
consume f (c : s) = fmap (,s) $ f c

choose :: Parser a -> Parser a -> Parser a
choose p q s = p s <|> q s

combine :: (a -> a -> a) -> Parser a -> Parser a -> Parser a
combine f p q s = liftA2 g (p s) (q s) 
	where g (v1, _) (v2, s) = (f v1 v2, s)
