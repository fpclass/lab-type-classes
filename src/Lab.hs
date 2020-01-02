--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type classes                                                          --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Semigroup(..), Monoid(..) )

--------------------------------------------------------------------------------
-- Semigroups

-- Semigroup law:
--
-- (Associativity)      x <> (y <> z) = (x <> y) <> z

class Semigroup a where 
    (<>) :: a -> a -> a 

instance Semigroup Int where 
    (<>) = undefined 

instance Semigroup [a] where 
    (<>) = undefined 

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mempty <> x = x
-- (Right identity)     x <> mempty = x
-- (mconcat)            mconcat = foldr (<>) mempty

class Semigroup a => Monoid a where
    mempty  :: a
    mconcat :: [a] -> a
    mconcat = undefined

instance Monoid Int where
    mempty  = undefined

instance Monoid [a] where
    mempty  = undefined

--------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where 
    (<>) = undefined 

instance Monoid b => Monoid (a -> b) where
    mempty  = undefined

--------------------------------------------------------------------------------
