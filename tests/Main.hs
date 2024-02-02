module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

-- https://stackoverflow.com/questions/8976488/quickcheck-exit-status-on-failures-and-cabal-integration

import CombinatoryLogic

instance Arbitrary Term where
    arbitrary = oneof [ return S, return K, App <$> arbitrary <*> arbitrary ]

prop_parsepretty :: Term -> Bool 
prop_parsepretty t = parseTerm (prettyTerm t) == Just t

main = do
    result <- quickCheckResult prop_parsepretty
    unless (isSuccess result) exitFailure
