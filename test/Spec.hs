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
    describe "Monoid instance for Int" $ do
        prop "satisfies the left identity" $ \(x :: Int) ->
            L.mappend L.mempty x == x
        prop "satisfies the right identity" $ \(x :: Int) ->
            L.mappend x L.mempty == x
        prop "mappend is associative" $ \(x :: Int) y z ->
            L.mappend x (L.mappend y z) == L.mappend (L.mappend x y) z
        prop "mconcat concatenates" $ \(xs :: [Int]) ->
            L.mconcat xs == foldr L.mappend L.mempty xs
    describe "Monoid instance for [a]" $ do
        prop "satisfies the left identity" $ \(x :: [Int]) ->
            L.mappend L.mempty x == x
        prop "satisfies the right identity" $ \(x :: [Int]) ->
            L.mappend x L.mempty == x
        prop "mappend is associative" $ \(x :: [Int]) y z ->
            L.mappend x (L.mappend y z) == L.mappend (L.mappend x y) z
        prop "mconcat concatenates" $ \(xs :: [[Int]]) ->
            L.mconcat xs == foldr L.mappend L.mempty xs

--------------------------------------------------------------------------------
