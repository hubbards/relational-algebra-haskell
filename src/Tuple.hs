-- | This module contains data types and operations for a nested tuple algebra.
--
module Tuple
  (
    -- * Tuple
      Tuple (..)
    , nil
    , nils
    , pair
    , triple
    , ntuple
    , pretty

    -- * Coordinate
    , Coord
    , safe

    -- * Zipper
    , Context (..)
    , Zipper
    , (.>)
    , run
    , update
    , enter
    , exit
    , up
    , down
    , coord

    -- * Structural Operations
    , proj
    , inj
    , trans
    , prod
    , join

  ) where

import Control.Monad hiding (join)

import Text.PrettyPrint

-- -----------------------------------------------------------------------------
-- Tuple

-- | Data type for a nested tuple.
--
data Tuple a = Atom a          -- atom
             | Tuple [Tuple a] -- tuple
  deriving Eq

-- | Empty tuple.
--
nil :: Tuple a
nil = Tuple []

-- | Tuple of nil.
nils :: Int -> Tuple a
nils = Tuple . flip replicate nil

-- | Smart constructor for pair.
--
pair :: a -> a -> Tuple a
pair x y = Tuple [Atom x, Atom y]

-- | Smart constructor for triple.
--
triple :: a -> a -> a -> Tuple a
triple x y z = Tuple [Atom x, Atom y, Atom z]

-- | Smart constructor for n-tuple.
--
ntuple :: [a] -> Tuple a
ntuple = Tuple . map Atom

-- | Show instance for tuple.
--
--   Note that a tuple cannot be constructed (directly) from an application of
--   the show method.
--
--   Examples of show:
--
--   >>> Atom 1
--   1
--
--   >>> triple 1 2 3
--   (1, 2, 3)
--
--   >>> Tuple [Atom 1, pair 2 3]
--   (1, (2, 3))
--
--   >>> Tuple [Atom 1, nil]
--   (1, Nil)
--
instance Show a => Show (Tuple a) where
  -- Show operation.
  show (Atom x)         = show x
  show (Tuple [])       = "Nil"
  show (Tuple (t : ts)) = "(" ++ show t ++ helper ts ++ ")" where
    helper []       = ""
    helper (t : ts) = ", " ++ show t ++ helper ts

-- | Pretty print function.
--
pretty :: Show a => Tuple a -> Doc
pretty (Atom x)         = text (show x)
pretty (Tuple [])       = text "Nil"
pretty (Tuple (t : ts)) = parens (pretty t <> helper ts) where
  helper []       = empty
  helper (t : ts) = comma <+> pretty t <> helper ts

-- | Monad instance for tuple.
--
instance Monad Tuple where
  -- Inject operation.
  return = Atom
  -- Bind operation.
  Atom x         >>= f = f x
  Tuple []       >>= _ = Tuple []
  Tuple (t : ts) >>= f = Tuple $ (t >>= f) : map (f =<<) ts

-- | Applicative functor instance for tuple.
--
instance Applicative Tuple where
  -- Inject operation.
  pure = return
  -- Apply operation.
  (<*>) = ap

-- | Functor instance for tuple.
--
instance Functor Tuple where
  -- Map operation.
  fmap = liftM

-- | Monoid instance for tuple.
--
instance Monoid (Tuple a) where
  -- Identity element.
  mempty = nil
  -- Law of composition.
  mappend = prod [] []

-- | Foldable instance for tuple.
--
instance Foldable Tuple where
  foldr f y (Atom x)         = f x y
  foldr _ y (Tuple [])       = y
  foldr f y (Tuple (t : ts)) = foldr f (foldr f y $ Tuple ts) t

-- -----------------------------------------------------------------------------
-- Coordinate

type Coord = [Int]

-- | Predicate for the definition of a "safe" coordinate for a tuple.
--
--   Examples of predicate:
--
--   >>> safe [0] (pair 1 2)
--   True
--
--   >>> safe [0, 1] (Tuple [pair 1 2, Atom 3])
--   True
--
--   >>> safe [1] (Tuple [Atom 1, nil])
--   True
--
--   >>> safe [2] (pair 1 2)
--   False
--
--   >>> safe [0] (Atom 1)
--   False
--
safe :: Coord -> Tuple a -> Bool
safe [] _                = True
safe (c : cs) (Tuple ts) = 0 <= c && c < length ts && safe cs (ts !! c)
safe _ _                 = False

-- Error for unsafe coordinate.
unsafe :: Coord -> a
unsafe c = error $ "unsafe coordinate: " ++ show c

-- Error for unsafe coordinates.
unsafes :: a
unsafes = error "unsafe coordinates"

-- -----------------------------------------------------------------------------
-- Zipper

data Context a = Hole
               | In [Tuple a] (Context a) [Tuple a]
  deriving Eq

type Zipper a = (Tuple a, Context a)

type Op1 a = Tuple a -> Tuple a
type Op2 a = Tuple a -> Op1 a

-- | Function composition with arguments flipped.
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

-- | Run zipper function on tuple.
run :: (Zipper a -> Zipper a) -> Op1 a
run f = enter .> f .> exit

-- | Update tuple at focus of zipper.
update :: Op1 a -> Zipper a -> Zipper a
update f (t, ctx) = (f t, ctx)

-- | Enter a zipper.
enter :: Tuple a -> Zipper a
enter t = (t, Hole)

-- | Exit a zipper.
exit :: Zipper a -> Tuple a
exit (t, Hole) = t
exit z         = exit (up z)

-- | Focus out one step (not total).
up :: Zipper a -> Zipper a
up (t, In ls c rs) = (Tuple $ ls ++ [t] ++ rs, c)

-- | Focus in on coordinate (not total).
down :: Int -> Zipper a -> Zipper a
down i (Tuple ts, c) = (head rs, In ls c $ tail rs) where
  (ls, rs) = splitAt i ts

-- | Focus in on coordinate (unsafe).
coord :: Coord -> Zipper a -> Zipper a
coord c z = if safe c (fst z) then helper c z else unsafe c where
  helper [] z                      = z
  helper (c : cs) z @ (Tuple _, _) = helper cs (down c z)

-- run tuple function at coordinate
run1 :: Op1 a -> Coord -> Op1 a
run1 f c = run (coord c .> update f)

-- run tuple function at coordinate
run2 :: Op2 a -> Coord -> Coord -> Op2 a
run2 f i j = (exit .) . on (update2 f) (coord i . enter) (coord j . enter)

-- generalize "on" function
on :: (b -> d -> e) -> (a -> b) -> (c -> d) -> a -> c -> e
on f g h x y = f (g x) (h y)

-- update and merge zippers
update2 :: Op2 a -> Zipper a -> Zipper a -> Zipper a
update2 f (tl, cl) (tr, cr) = (f tl tr, merge cl cr)

-- merge two contexts; interleaving is not swapped on left
merge :: Context a -> Context a -> Context a
merge Hole c                    = c
merge c Hole                    = c
merge (In ll l rl) (In lr r rr) = In (ll ++ lr) (merge l r) (rl ++ rr)

-- -----------------------------------------------------------------------------
-- Structural Operations

-- | Projection operation (unsafe).
--
--   This function takes a coordinate and a tuple as parameters and projects
--   the tuple onto the coordinate.
--
--   Examples of projection:
--
--   >>> proj [0] (pair 1 2)
--   1
--
--   >>> proj [2] (triple 1 2 3)
--   3
--
--   >>> proj [0] (Tuple [pair 1 2, Atom 3])
--   (1, 2)
--
--   >>> proj [0, 0] (Tuple [pair 1 2, Atom 3])
--   1
--
proj :: Coord -> Op1 a
proj c = enter .> (coord c) .> fst

-- | Injection operation (unsafe).
--
--   This function takes a coordinate and two tuples as parameters and injects
--   the second tuple into the "hole" at the coordinate in the first tuple.
--
--   Examples of injection:
--
--   >>> inj [0] (pair 1 2) (Atom 3)
--   (3, 2)
--
--   >>> inj [2] (Tuple [Atom 1, Atom 2, nil]) (Atom 3)
--   (1, 2, 3)
--
--   >>> inj [0] (pair 1 2) (pair 3 4)
--   ((3, 4), 2)
--
--   >>> inj [1, 0] (Tuple [Atom 1, pair 2 3]) (Atom 4)
--   (1, (4, 3))
--
inj :: Coord -> Op2 a
inj c l r = run1 (const r) c l

-- | Transposition operation (unsafe).
--
--   This function takes two coordinates and a tuple as parameters and
--   transposes the coordinates in the tuple.
--
--   Examples:
--
--   >>> trans [0] [1] (pair 1 2)
--   (2, 1)
--
--   >>> trans [0, 0] [1] (Tuple [pair 1 2, Atom 3])
--   ((3, 2), 1)
--
trans :: Coord -> Coord -> Op1 a
trans i j t = inj j (inj i t $ proj j t) (proj i t)

-- | Product operation for tuples (unsafe).
--
--   This function takes two coordinates and tuples as parameters and forms the
--   product of the tuples at the coordinates. Coordinates on the right and left
--   sides of the path to the product coordinates are interleaved. Interleaving
--   is not swapped on the left side.
--
--   Simple examples of product:
--
--   >>> let op = prod [] []
--
--   >>> Atom 1 `op` Atom 2
--   (1, 2)
--
--   >>> pair 1 2 `op` Atom 3
--   (1, 2, 3)
--
--   >>> pair 1 2 `op` nil
--   (1, 2)
--
--   More complex examples of product:
--
--   >>> let t1 = Tuple [pair 1 2, Atom 3]
--   >>> let t2 = Tuple [Atom 4, pair 5 6]
--
--   >>> let c1 = [0]
--   >>> let c2 = [1]
--
--   >>> prod c1 c2 t1 t2
--   (4, (1, 2, 5, 6), 3)
--
--   TODO: add more doctests
--
prod :: Coord -> Coord -> Op2 a
prod i j = run2 helper i j where
  helper (Tuple []) r          = r
  helper l (Tuple [])          = l
  helper (Tuple ls) (Tuple rs) = Tuple $ ls ++ rs
  helper (Tuple ls) r          = Tuple $ ls ++ [r]
  helper l (Tuple rs)          = Tuple $ l : rs
  helper l r                   = Tuple [l, r]

-- | Join operation for tuples (unsafe).
--
--   This function takes two coordinates and tuples as parameters and joins the
--   tuples on the matching coordinates. Coordinates on the right and left sides
--   of the path to the matching coordinates are interleaved. Interleaving is
--   not swapped on the left side.
--
--   Simple examples of join:
--
--   >>> join [1] [0] (pair 1 2) (pair 2 1)
--   (1, 2, 1)
--
--   >>> join [0] [1] (pair 1 2) (pair 2 1)
--   (2, 1, 2)
--
--   >>> join [1] [1] (triple 1 2 3) (triple 4 2 5)
--   (1, 4, 2, 3, 5)
--
--   More complex examples of join:
--
--   >>> let t1 = Tuple [Atom 1, triple 2 3 4, Atom 5]
--   >>> let t2 = Tuple [Atom 6, triple 7 3 8, Atom 9]
--   >>> let t3 = Tuple [Atom 6, triple 3 7 8, Atom 9]
--   >>> let t4 = Tuple [Atom 6, triple 7 8 3, Atom 9]
--   >>> let t5 = Tuple [triple 3 7 8, Atom 6, Atom 9]
--   >>> let t6 = Tuple [Atom 6, Atom 9, triple 7 8 3]
--
--   >>> let c1 = [1, 1]
--   >>> let c3 = [1, 0]
--   >>> let c4 = [1, 2]
--   >>> let c5 = [0, 0]
--   >>> let c6 = [2, 2]
--
--   >>> join c1 c1 t1 t2
--   (1, 6, (2, 7, 3, 4, 8), 5, 9)
--
--   >>> join c1 c3 t1 t3
--   (1, 6, (2, 3, 4, 7, 8), 5, 9)
--
--   >>> join c1 c4 t1 t4
--   (1, 6, (2, 7, 8, 3, 4), 5, 9)
--
--   >>> join c1 c5 t1 t5
--   (1, (2, 3, 4, 7, 8), 5, 6, 9)
--
--   >>> join c1 c6 t1 t6
--   (1, 6, 9, (2, 7, 8, 3, 4), 5)
--
join :: Eq a => Coord -> Coord -> Op2 a
join i j = run2 helper i j where
  helper l r
    | l == r    = l
    | otherwise = error "coordinates don't match"

-- TODO: add join and product where interleaving is swapped on the left
