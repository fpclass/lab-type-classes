--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type classes                                                          --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Lab as L

--------------------------------------------------------------------------------

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "Semigroup instance for Int" $ do 
        prop "(<>) is associative" $ \(x :: Int) y z ->
            (L.<>) x ((L.<>) y z) == (L.<>) ((L.<>) x y) z
    describe "Semigroup instance for [a]" $ do 
        prop "(<>) is associative" $ \(x :: [Int]) y z ->
            (L.<>) x ((L.<>) y z) == (L.<>) ((L.<>) x y) z
    describe "Monoid instance for Int" $ do
        prop "satisfies the left identity" $ \(x :: Int) ->
            (L.<>) L.mempty x == x
        prop "satisfies the right identity" $ \(x :: Int) ->
            (L.<>) x L.mempty == x
    describe "Monoid instance for [a]" $ do
        prop "satisfies the left identity" $ \(x :: [Int]) ->
            (L.<>) L.mempty x == x
        prop "satisfies the right identity" $ \(x :: [Int]) ->
            (L.<>) x L.mempty == x

--------------------------------------------------------------------------------
