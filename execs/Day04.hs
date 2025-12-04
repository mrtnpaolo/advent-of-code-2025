module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.List.Split    qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputArray 4
     print (part1 inp)
     print (part2 inp)

part1 = length . accessible

accessible a =
  [ c
  | (c,'@') <- A.assocs a
  , length [ n | n <- neighbors c, '@' <- maybeToList (a A.!? n) ] < 4 ]

part2 = sum . L.unfoldr remove

remove a
  | null cs   = Nothing
  | otherwise = Just (length cs,a')
  where
    cs = accessible a
    a' = a A.// [ (c,'.') | c <- cs ]
