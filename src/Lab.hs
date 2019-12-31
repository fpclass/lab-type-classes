--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type classes                                                          --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Monoid(..) )

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
