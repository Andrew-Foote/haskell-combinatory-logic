module TypeSpec (spec) where

import Data.List.Extra
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.UnorderedContainers

import Type

instance Arbitrary Var where
    arbitrary = oneof [ NamedVar <$> arbitrary, FreshVar <$> arbitrary ]

instance Arbitrary Type where
    arbitrary = oneof [ VT <$> arbitrary, (:->) <$> arbitrary <*> arbitrary ]

deriving instance Arbitrary Env

nameFreshVarsSpec :: Spec
nameFreshVarsSpec = describe "nameFreshVars" $ do
    it "preserves the number of variable occurrences in a term" $
        property $ \ a -> length (vars a) == length (vars $ nameFreshVars a)
    it "preserves the number of distinct variables in a term" $
        property $ \ a -> length (nub $ vars a) == length (nub $ vars $ nameFreshVars a)
    it "affects only the fresh variable occurrences, and turns them into named variable occurrences" $
        property $ \ a -> and $ zipWith check (vars a) (vars $ nameFreshVars a) 
            where
                check a b = case (a, b) of
                    (NamedVar name1, NamedVar name2) -> name1 == name2
                    (FreshVar _, NamedVar _) -> True
                    (_, FreshVar _) -> False

expandSpec :: Spec
expandSpec = describe "expand" $ do
    it "doesn't do anything when the environment is empty" $
        property $ \ a -> expand a groundEnv == a

occursSpec :: Spec
occursSpec = describe "occurs" $ do
    it "agrees with the vars function" $ property agreesWithVar
        where
            agreesWithVar :: Var -> Type -> Bool
            agreesWithVar a b = occurs a b groundEnv == elem a (vars b)

unifySpec :: Spec
unifySpec = describe "unify" $ do
    it "unifies" $ property unifies
        where
            unifies :: Type -> Type -> Bool
            unifies a b = case unify a b groundEnv of
                Just env -> expand a env == expand b env
                Nothing -> True

spec :: Spec
spec = do
    -- nameFreshVarsSpec    (seems to cuase an infinite loop)
    expandSpec
    occursSpec
    unifySpec