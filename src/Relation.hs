-- | This module contains data types and operations for an extension of
--   relational algebra. The extension allows relations that are not in first
--   normal form, i.e., nested relations.
--
--   Note that attribute names are given by coordinates. Also, note that
--   relation keys and schemas are omitted.
--
module Relation
  (
    -- * Relation
      Relation
    , pretty

    -- * Set-Based Operations
    , union
    , diff
    , inter
    , select

    -- * Structural Operations
    , proj
    , inj
    , trans
    , prod
    , join

  ) where

import qualified Data.List as L

import Text.PrettyPrint

import qualified Tuple as T

{- $setup

Example relations for doctests:

>>> let rs1 = [T.pair 1 2, T.pair 3 4] :: Relation Int
>>> let rs2 = [T.pair 2 1, T.pair 3 4] :: Relation Int

>>> let x1 = T.Tuple [T.Atom 1, T.pair 2 3] :: T.Tuple Int
>>> let x2 = T.Tuple [T.Atom 4, T.pair 5 6] :: T.Tuple Int
>>> let y1 = T.Tuple [T.pair 1 2, T.Atom 3] :: T.Tuple Int
>>> let y2 = T.Tuple [T.pair 7 8, T.Atom 9] :: T.Tuple Int
>>> let z1 = T.Tuple [T.pair 3 2, T.Atom 1] :: T.Tuple Int

>>> let xs = [x1, x2] :: Relation Int
>>> let ys = [y1, y2] :: Relation Int
>>> let zs = z1 : ys :: Relation Int

-}

-- -----------------------------------------------------------------------------
-- Relation

-- | A relation is a bag of tuples (of the same arity).
type Relation a = [T.Tuple a]

type Op1 a = Relation a -> Relation a
type Op2 a = Relation a -> Op1 a

pretty :: Show a => Relation a -> Doc
pretty = vcat . (map T.pretty)

-- TODO: add type checking
-- TODO: make newtype and implement Eq and Show typeclasses

-- -----------------------------------------------------------------------------
-- Set-Based Operations

-- | Higher-order helper function for set-based operations that removes
--   duplicates from relations.
rdup :: Eq a => Op2 a -> Op2 a
rdup op l r = L.nub l `op` L.nub r

-- | Union operation.
--
--   Examples of union:
--
--   >>> rs1 `union` rs2
--   [(1, 2),(3, 4),(2, 1)]
--
union :: Eq a => Op2 a
union = rdup L.union

-- | Difference operation.
--
--   Examples of difference:
--
--   >>> rs1 `diff` rs2
--   [(1, 2)]
--
--   >>> rs2 `diff` rs1
--   [(2, 1)]
--
diff :: Eq a => Op2 a
diff = rdup (L.\\)

-- | Intersection operation.
--
--   Examples of intersection:
--
--   >>> rs1 `inter` rs2
--   [(3, 4)]
--
--   >>> rs2 `inter` rs1
--   [(3, 4)]
--
inter :: Eq a => Op2 a
inter = rdup L.intersect

-- | Selection operation.
--
--   Examples of selection:
--
--   >>> select (T.safe [1, 0]) xs
--   [(1, (2, 3)),(4, (5, 6))]
--
--   >>> select (T.safe [1, 0]) ys
--   []
--
select :: (T.Tuple a -> Bool) -> Op1 a
select = filter

-- -----------------------------------------------------------------------------
-- Structural Operations

-- | Projection operation.
--
--   Examples of projection:
--
--   >>> let pi = proj [1]
--
--   >>> pi xs
--   [(2, 3),(5, 6)]
--
--   >>> pi ys
--   [3,9]
--
--   >>> proj [1, 0] xs
--   [2,5]
--
proj :: T.Coord -> Op1 a
proj c = map (T.proj c)

-- | Injection operation.
--
--   Examples of injection:
--
--   >>> let iota = inj [1] (T.Tuple [T.Atom 1, T.nil])
--
--   >>> iota rs1
--   [(1, (1, 2)),(1, (3, 4))]
--
--   >>> iota rs2
--   [(1, (2, 1)),(1, (3, 4))]
--
inj :: T.Coord -> T.Tuple a -> Op1 a
inj c r = map (T.inj c r)

-- | Transposition operation.
--
--   TODO: doctests
--
trans :: T.Coord -> T.Coord -> Op1 a
trans ci cj = map (T.trans ci cj)

-- | Product operation for relations.
--
--   Simple examples of product:
--
--   >>> let op = prod [] []
--
--   >>> rs1 `op` rs2
--   [(1, 2, 2, 1),(1, 2, 3, 4),(3, 4, 2, 1),(3, 4, 3, 4)]
--
--   >>> rs2 `op` rs1
--   [(2, 1, 1, 2),(2, 1, 3, 4),(3, 4, 1, 2),(3, 4, 3, 4)]
--
--   More complex examples of product:
--
--   TODO: doctests
--
prod :: T.Coord -> T.Coord -> Op2 a
prod cl cr l r = map (T.prod cl cr) l <*> r

-- | Join operation for relations. Note that this is a natural join.
--
--   Simple examples of join:
--
--   TODO: doctests
--
--   More complex examples of join:
--
--   >>> let op = join [1, 0] [0, 1]
--
--   >>> xs `op` ys
--   [(1, (1, 2, 3), 3)]
--
--   >>> xs `op` zs
--   [(1, (3, 2, 3), 1),(1, (1, 2, 3), 3)]
--
join :: Eq a => T.Coord -> T.Coord -> Op2 a
join cl cr l r =
  let
    pl = zip l (proj cl l)
    pr = zip r (proj cr r)
  in
    [T.join cl cr xl xr | (xl, yl) <- pl, (xr, yr) <- pr, yl == yr]
