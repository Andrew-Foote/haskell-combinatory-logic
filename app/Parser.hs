module Parser where

import Control.Applicative 
import Control.Monad

newtype Parser a = Parser { unParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap = liftA

instance Applicative Parser where
    pure v = Parser $ \ s -> pure (v, s)
    (<*>) = ap

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= f = Parser $ \ s -> p s >>= \case (v, s1) -> unParser (f v) s1

instance Alternative Parser where
    empty = Parser $ const empty
    Parser p <|> Parser q = Parser $ \ s -> p s <|> q s

charParser :: (Char -> Maybe a) -> Parser a
charParser f = Parser $ \case
    c : s -> (, s) <$> f c
    _ -> Nothing

consumeChar :: Char -> Parser ()
consumeChar c = charParser $ \ c1 -> if c == c1 then Just () else Nothing