--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type classes                                                          --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Semigroup(..), Monoid(..), concat, sum, product )

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
    (<>) = (*) 

-- instance Semigroup Int where 
--     (<>) = (+) 

instance Semigroup [a] where
    -- (<>) :: [a] -> [a] -> [a] 
    (<>) = (++) 

newtype Sum = MkSum Int
    deriving Show

unSum :: Sum -> Int
unSum (MkSum x) = x

newtype Product = MkProduct Int
    deriving Show

unProduct :: Product -> Int
unProduct (MkProduct x) = x

instance Semigroup Sum where 
    (MkSum x) <> (MkSum y) = MkSum (x+y)

instance Semigroup Product where 
    (MkProduct x) <> (MkProduct y) = MkProduct (x*y)

combine :: Semigroup a => [a] -> a 
combine [x] = x
combine (x:xs) = x <> combine xs

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mempty <> x = x
-- (Right identity)     x <> mempty = x

class Semigroup a => Monoid a where
    mempty :: a

instance Monoid Int where
    mempty  = 1

-- instance Monoid Int where
--     mempty  = 0

instance Monoid Sum where 
    mempty = MkSum 0

instance Monoid Product where 
    mempty = MkProduct 1

instance Monoid [a] where
    mempty  = []

-- | `mconcat` @xs@ combines all the elements in @xs@ using the `(<>)` operator
-- from the `Semigroup` instance for the type of elements in @xs@.
mconcat :: Monoid a => [a] -> a
mconcat = foldr (<>) mempty
-- mconcat []     = mempty
-- mconcat (x:xs) = x <> mconcat xs

concat :: [[a]] -> [a]
concat = mconcat 

sum :: [Int] -> Int
sum xs = unSum (mconcat (map MkSum xs))

product :: [Int] -> Int
product xs = unProduct (mconcat (map MkProduct xs))

--------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where 
    -- (<>) :: Semigroup b => (a -> b) -> (a -> b) -> a -> b
    (f <> g) x = f x <> g x

--    ((\x -> x ++ [1,2]) <> (\x -> x ++ [3,4])) [5]
-- => ((\x -> x ++ [1,2]) [5]) <> ((\x -> x ++ [3,4]) [5])
-- => ([5] ++ [1,2]) <> ((\x -> x ++ [3,4]) [5])
-- => [5,1,2] <> ((\x -> x ++ [3,4]) [5])
-- => [5,1,2] <> ([5] ++ [3,4])
-- => [5,1,2] <> [5,3,4]
-- => [5,1,2,5,3,4]

instance Monoid b => Monoid (a -> b) where
    -- mempty :: Monoid b => a -> b
    mempty x = mempty

--------------------------------------------------------------------------------
