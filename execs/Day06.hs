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
  do inp1 <- getInput (map parse1 . pre1) 6
     print (part1 inp1)
     inp2 <- getInput (map parse2 . pre2) 6
     print (part2 inp2)
  where
    pre1            = L.transpose . reverse . map words . lines
    parse1 ([f]:xs) = (fn f,map read xs)

    pre2              = map (reverse . L.transpose)
                      . L.splitWhen (all (' '==)) . L.transpose . lines
    parse2 ((f:_):xs) = (fn f,map read . L.transpose . reverse $ xs)

    fn '*' = product
    fn '+' = sum

part1 = sum . map (uncurry ($))

part2 = part1

