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
  do inp <- getInputLines parse 1
     print (part1 inp)
     print (map ((`mod` 100) . last) $ scanl g [50] inp)
     print (part2 inp)
  where
    parse ('L':n) = -(read @Int n)
    parse ('R':n) = (read @Int n)

part1 = count (0==) . scanl f 50

f n m = (n + m) `mod` 100

part2 = length . concat . map (filter (0==) . map (`mod` 100)) . scanl g [50]

g xs n = map (`mod` 100) . take (abs n) $ tail $ iterate (+ (signum n)) m
  where
    m = last xs `mod` 100

