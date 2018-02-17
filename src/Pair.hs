-- | This module contains data types and operations for a nested pair algebra.
--
module Pair
  (
    -- * Pair
      Pair (..)
    , pair
    , triple
    , ntuple
    , fold
    
    -- * Coordinate
    , Coord (..)
    , safe
    
    -- * Zipper
    
    -- * Structural Operations
    , proj
    , inj
    
    -- * Compositional Operations
    , prod
    , join
    
  ) where

import Control.Monad hiding (join)

-- -----------------------------------------------------------------------------
-- Pair

-- | Data type for a nested pair.
--
data Pair a = Nil                     -- empty
             | Atom a                 -- atom
             | Pair (Pair a) (Pair a) -- pair
  deriving Eq

-- | Smart constructor for pair.
--
pair :: a -> a -> Pair a
pair x y = Pair (Atom x) (Atom y)

-- | Smart constructor for triple.
--
triple :: a -> a -> a -> Pair a
triple x y z = Pair (Atom x) $ Pair (Atom y) (Atom z)

-- | Smart constructor for n-tuple.
--
ntuple :: [a] -> Pair a
ntuple []       = Nil
ntuple (x : xs) = Pair (Atom x) (ntuple xs)

-- | Show instance for pair.
--
--   Note that this function prints an encoding of tuples as right nested pairs.
--   This means a pair cannot be constructed (directly) from an application of
--   this function.
--
--   Examples of show:
--
--   >>> Pair (Atom 1) (Atom 2)
--   ( 1 , 2 )
--
--   >>> Pair (Atom 1) (Pair (Atom 2) Nil)
--   ( 1 , 2 )
--
--   >>> Pair (Pair (Atom 1) (Atom 2)) (Atom 3)
--   ( ( 1 , 2 ) , 3 )
--
--   >>> Pair (Atom 1) (Pair (Atom 2) (Atom 3))
--   ( 1 , 2 , 3 )
--
--   >>> Pair (Atom 1) (Pair (Pair (Atom 2) (Atom 3)) Nil)
--   ( 1 , ( 2 , 3 ) )
--
--   >>> Pair (Atom 1) (Pair Nil (Atom 2))
--   ( 1 , Nil , 2 )
--
instance Show a => Show (Pair a) where
  -- Show method.
  show Nil        = "Nil"
  show (Atom x)   = show x
  show (Pair l r) = "( " ++ show l ++ helper r ++ " )" where
    helper Nil        = ""
    helper (Atom x)   = " , " ++ show x
    helper (Pair l r) = " , " ++ show l ++ helper r

-- | Monad instance for pair.
--
--   TODO: write doctests for monad laws
--
instance Monad Pair where
  -- Inject operation.
  return = Atom
  -- Bind operation.
  Nil      >>= _ = Nil
  Atom x   >>= f = f x
  Pair l r >>= f = Pair (l >>= f) (r >>= f)

-- | Applicative functor instance for pair.
--
--   TODO: write doctests for applicative functor laws
--
instance Applicative Pair where
  -- Inject operation.
  pure = return
  -- Apply operation.
  (<*>) = ap

-- | Functor instance for pair.
--
--   TODO: write doctests for functor laws
--
instance Functor Pair where
  -- Map operation.
  fmap = liftM

-- | Fold operation (safe).
--
--   This function takes parameters for each constructor of the pair data
--   type and folds them over a pair. Note that this operation cannot be used to
--   implement an instance of 'Foldable'.
--
--   TODO: write doctests
--
fold :: (a -> b) -> (b -> b -> b) -> b -> Pair a -> b
fold _ _ y Nil        = y
fold f _ _ (Atom x)   = f x
fold f g y (Pair l r) = g (fold f g y l) (fold f g y r)

-- -----------------------------------------------------------------------------
-- Coordinate

-- | Data type for describing a coordinate in a nested pair.
--
data Coord = Id        -- identity
           | InL Coord -- left
           | InR Coord -- right
  deriving (Eq, Show)

-- | Predicate for the definition of a safe coordinate for a pair.
--
--   Examples of predicate:
--
--   >>> safe (InL Id) (Pair (Atom 1) (Atom 2))
--   True
--
--   >>> safe (InR $ InR Id) (Pair (Atom 1) (Pair (Atom 2) Nil))
--   True
--
--   >>> safe (InR $ InR Id) (Pair (Atom 1) (Atom 2))
--   False
--
safe :: Coord -> Pair a -> Bool
safe Id _               = True
safe (InL c) (Pair l _) = safe c l
safe (InR c) (Pair _ r) = safe c r
safe _ _                = False

-- Error for unsafe coordinate.
unsafe :: Coord -> a
unsafe c = error $ "Unsafe coordinate: " ++ show c

-- -----------------------------------------------------------------------------
-- Zipper

-- TODO: add zipper

-- -----------------------------------------------------------------------------
-- Structural Operations

-- | Projection operation (unsafe).
--
--   This function takes a coordinate and a pair as parameters and projects
--   the pair onto the coordinate.
--
--   Examples of projection:
--
--   >>> proj (InR Id) (Pair (Atom 1) (Atom 2))
--   2
--
--   >>> proj (InR Id) (Pair (Atom 1) (Pair (Atom 2) Nil))
--   ( 2 )
--
--   >>> proj (InR $ InR Id) (Pair (Atom 1) (Pair (Atom 2) Nil))
--   Nil
--
--   >>> proj (InL Id) (Pair (Pair (Atom 1) (Atom 2)) (Atom 3))
--   ( 1 , 2 )
--
proj :: Coord -> Pair a -> Pair a
proj Id p               = p
proj (InL c) (Pair l _) = proj c l
proj (InR c) (Pair _ r) = proj c r
proj c _                = unsafe c

-- | Injection operation (unsafe).
--
--   This function takes a coordinate and two pairs as parameters and injects
--   the second pair into the "hole" at the coordinate in the first pair.
--
--   Examples of injection:
--
--   >>> inj (InL Id) (Pair (Atom 1) (Atom 2)) (Atom 3)
--   ( 3 , 2 )
--
--   >>> inj (InR $ InR Id) (Pair (Atom 1) (Pair (Atom 2) Nil)) (Atom 3)
--   ( 1 , 2 , 3 )
--
--   >>> inj (InL Id) (Pair (Atom 1) (Atom 2)) (Pair (Atom 3) (Atom 4))
--   ( ( 3 , 4 ) , 2 )
--
inj :: Coord -> Pair a -> Pair a -> Pair a
inj Id _ p               = p
inj (InL c) (Pair l r) p = Pair (inj c l p) r
inj (InR c) (Pair l r) p = Pair l (inj c r p)
inj c _ _                = unsafe c

-- -----------------------------------------------------------------------------
-- Compositional Operations

-- | Product operation.
--
--   TODO: write doctests
--
prod :: Pair a -> Pair a -> Pair a
prod = undefined

-- TODO: use zippers to implement join

-- | Join operation (unsafe).
--
--   TODO: write doctests
--
join :: Eq a => Coord -> Coord -> Pair a -> Pair a -> Pair a
join = undefined
