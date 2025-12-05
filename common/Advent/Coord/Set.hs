module Advent.Coord.Set
  ( Set
  -- * Construction
  , fromSet
  , fromSetR
  -- * Query
  , size
  , member
  , notMember
  , toList
  -- * Modify
  , delete
  , filter
  -- * Neighbors
  , cardinal
  , neighbors
  ) where

import Prelude hiding (filter)
import Data.Set qualified as S
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Advent.Coord (Coord(..), coordRow, coordCol)

data Set = CS !Int !IntSet

-- Construction

fromSet :: S.Set Coord -> Set
fromSet = fromSetR 1

fromSetR :: Int -> S.Set Coord -> Set
fromSetR r cs = CS w (IS.fromList [enc c | c <- S.toList cs])
  where
    w = 1 + r + maximum (0 : [coordCol c | c <- S.toList cs])
    enc c = coordRow c * w + coordCol c

-- Query

size :: Set -> Int
size (CS _ s) = IS.size s

member :: Int -> Set -> Bool
member i (CS _ s) = IS.member i s

notMember :: Int -> Set -> Bool
notMember i (CS _ s) = IS.notMember i s

toList :: Set -> [Int]
toList (CS _ s) = IS.toList s

-- Modify

delete :: Int -> Set -> Set
delete i (CS w s) = CS w (IS.delete i s)

filter :: (Int -> Bool) -> Set -> Set
filter f (CS w s) = CS w (IS.filter f s)

-- Neighbors

cardinal :: Set -> Int -> [Int]
cardinal (CS w _) i = [i-w, i-1, i+1, i+w]

neighbors :: Set -> Int -> [Int]
neighbors (CS w _) i = [i-w-1, i-w, i-w+1, i-1, i+1, i+w-1, i+w, i+w+1]
