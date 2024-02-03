module Main where

import System.Exit
import System.IO

import CombinatoryLogic
import qualified Type

processInput :: String -> IO ()

processInput (':' : 'q' : _) = exitSuccess

processInput (':' : 't' : s) = case dropWhile (/= ' ') s of
    ' ' : s1 -> case parseTerm s1 of
        Just t -> case principalType t of
            Just a -> putStrLn $ Type.prettyType a
            Nothing -> putStrLn "! term not typable"
        Nothing -> putStrLn "! input not parsable as term"
    _ -> putStrLn "! no term given to type"

processInput (':' : _) = putStrLn "! unrecognised interpreter directive"

processInput s = case parseTerm s of
    Just t -> putStrLn $ prettyTerm $ reduce t
    Nothing -> putStrLn "! input not parsable as term"

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    s <- getLine
    processInput s
    main