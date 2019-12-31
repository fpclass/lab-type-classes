--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 3: Recursive and higher-order functions                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Monoid(..), elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem = undefined

maximum :: Ord a => [a] -> a
maximum = undefined

intersperse :: a -> [a] -> [a]
intersperse = undefined

any :: (a -> Bool) -> [a] -> Bool
any = undefined

all :: (a -> Bool) -> [a] -> Bool
all = undefined

flip :: (a -> b -> c) -> b -> a -> c
flip = undefined

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = undefined

subsequences :: [a] -> [[a]]
subsequences = undefined

permutations :: Eq a => [a] -> [[a]]
permutations = undefined

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mappend mempty x = x
-- (Right identity)     mappend x mempty = x
-- (Associativity)      mappend x (mappend y z) = mappend (mappend x y) z
-- (mconcat)            mconcat = foldr mappend mempty

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = undefined

instance Monoid Int where
    mempty  = undefined
    mappend = undefined

instance Monoid [a] where
    mempty  = undefined
    mappend = undefined

instance Monoid b => Monoid (a -> b) where
    mempty  = undefined
    mappend = undefined

--------------------------------------------------------------------------------
