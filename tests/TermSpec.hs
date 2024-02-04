module TermSpec (spec) where

import Control.Monad
import System.Exit
import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck

import Term ( Term(..), reduce1, reduce, prettyTerm, parseTerm )

instance Arbitrary Term where
    arbitrary = oneof [ return S, return K, App <$> arbitrary <*> arbitrary ]

iI = App (App S K) K
mM = App (App S iI) iI
bB = App (App S (App K S)) K

reduce1Spec :: Spec
reduce1Spec = describe "reduce1" $ do
    it "reduces MM correctly" $ reduce1 (App mM mM) == Just (App (App iI mM) (App iI mM))

{- we can't make these safely into property tests, since the arbitrary terms generated
may lack normal forms!
-}

reductionWithICorrect :: Term -> Bool
reductionWithICorrect t = reduce (App iI t) == reduce t

reductionWithBCorrect :: Term -> Term -> Term -> Bool
reductionWithBCorrect t u v = reduce (App (App (App bB t) u) v) == reduce (App t (App u v))

reduceSpec :: Spec
reduceSpec = describe "reduce" $ do
    doesntReduce S
    doesntReduce K
    doesntReduce $ App S K
    doesntReduce $ App S $ App S $ App S $ App S S
    it "reduces terms with I as the head correctly" $
        reductionWithICorrect S
    it "reduces terms with B as the head correctly" $
        reductionWithBCorrect S K (App S K)
  where
    doesntReduce t = do
        it ("doesn't reduce " ++ show t) $ reduce t == t

parseTermSpec :: Spec
parseTermSpec = describe "parseTerm" $ do
    it "is a left inverse of prettyTerm" $
        property $ parseInvertsPretty
  where parseInvertsPretty t = parseTerm (prettyTerm t) == Just t

spec :: Spec
spec = do
    reduce1Spec
    reduceSpec
    parseTermSpec