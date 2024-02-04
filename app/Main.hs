module Main where

import System.Exit
import System.IO

import qualified Term
import qualified Type

processInput :: String -> IO ()

processInput (':' : 'q' : _) = exitSuccess

processInput (':' : 't' : s) = case dropWhile (/= ' ') s of
    ' ' : s1 -> case Term.parseTerm s1 of
        Just t -> case Term.principalType t of
            Just a -> putStrLn $ Type.prettyType a
            Nothing -> putStrLn "! term not typable"
        Nothing -> putStrLn "! input not parsable as term"
    _ -> putStrLn "! no term given to type"

processInput (':' : _) = putStrLn "! unrecognised interpreter directive"

processInput s = case Term.parseTerm s of
    Just t -> putStrLn $ Term.prettyTerm $ Term.reduce t
    Nothing -> putStrLn "! input not parsable as term"

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    s <- getLine
    processInput s
    main