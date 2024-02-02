module Main where

import System.IO
import CombinatoryLogic

repl :: String -> String
repl s = case parseTerm s of
    Just t -> prettyTerm (reduce t)
    Nothing -> "! I couldn't parse that input."

main :: IO ()
main = 
    putStr "> "
    >> hFlush stdout
    >> getLine
    >>= \ s ->
        putStrLn (repl s)
        >> main