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
    -- | The <> operator is some binary operator (i.e. taking two arguments)
    -- which returns a value of the same type as the arguments and which obeys
    -- the above associativity law. 
    (<>) :: a -> a -> a 

-- There are at least two possible implementations of the Semigroup type class
-- for the Int type. However, Haskell's type class mechanism only allows one
-- instance of each type class for each type. Therefore, for the purpose of
-- this lab, you can arbitrarily decide on one of them. In the standard library,
-- there is no instance of Semigroup for Int for that reason. Instead, the
-- standard library uses a trick to work around the above limitation which
-- we will be able to understand further into the module.
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

class Semigroup a => Monoid a where
    mempty :: a

instance Monoid Int where
    mempty  = undefined

instance Monoid [a] where
    mempty  = undefined

-- | `mconcat` @xs@ combines all the elements in @xs@ using the `(<>)` operator
-- from the `Semigroup` instance for the type of elements in @xs@.
mconcat :: Monoid a => [a] -> a
mconcat []     = mempty
mconcat (x:xs) = x <> mconcat xs

--------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where 
    (<>) = undefined 

instance Monoid b => Monoid (a -> b) where
    mempty  = undefined

--------------------------------------------------------------------------------
