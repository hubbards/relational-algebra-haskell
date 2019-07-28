-- | This module contains a complete example for the extension of relational
--   algebra.
--
--   Suppose that a store sells computers assembled from used parts. The store
--   has a database for the inventory of used parts. In the database, there is
--   a relation for memory cards and a relation for motherboards with two used
--   memory slots.
--
module Example where

import Text.PrettyPrint

import qualified Tuple as T
import qualified Relation as R

-- | Relation for memory cards with attributes for slot type and capacity.
cards :: R.Relation String
cards = [
    T.pair "DDR2" "2"
  , T.pair "DDR2" "4"
  , T.pair "DDR3" "16"
  , T.pair "DDR3" "32"
  , T.pair "DDR4" "8"
  , T.pair "DDR4" "16"
  , T.pair "DDR4" "32"
  ]

-- | Relation for motherboards with two (composite) attributes for used memory
--   slots.
boards :: R.Relation String
boards = [
    T.Tuple [T.pair "DDR2" "2", T.pair "DDR3" "16"]
  , T.Tuple [T.pair "DDR2" "2", T.pair "DDR3" "32"]
  , T.Tuple [T.pair "DDR3" "16", T.pair "DDR4" "16"]
  , T.Tuple [T.pair "DDR4" "8", T.pair "DDR2" "4"]
  ]

-- | Semantic names for coordinates of some attributes of the memory card and
--   motherboard relations.
stype, used1, used2, stype1, stype2 :: T.Coord
stype = [0]
used1 = [0]
used2 = [1]
stype1 = used1 ++ stype
stype2 = used2 ++ stype

-- | Which computers can be assembled from the memory cards and motherboards so
--   that three memory slots are used by at most two slot types?
--
--   Assume that a motherboard has a free slot of a given type if a slot of that
--   type is already being used. Also, assume that memory cards already in a
--   motherboard can't be removed.
ex :: Doc
ex = R.pretty tmp4
  where
    tmp1 = R.join stype stype1 cards boards
    tmp2 = R.join stype stype2 cards boards
    tmp3 = R.trans used1 used2 tmp2
    tmp4 = tmp1 `R.union` tmp3
