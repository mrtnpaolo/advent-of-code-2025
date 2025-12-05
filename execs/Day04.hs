module Main (main) where

import Advent           (getInputMap)
import Advent.Coord.Set qualified as CS
import Data.Map         qualified as M

k = 4

main =
  do inp <- getInputMap 4
     let s = CS.fromSet . M.keysSet . M.filter ('@'==) $ inp
         q = [i | i <- CS.toList s, length (live s i) < k]
     print (part1 q)
     print (part2 s q)

part1 = length

part2 s q = CS.size s - CS.size (kcore s q)

live s c = [n | n <- CS.neighbors s c, CS.member n s]

-- k-core in linear time https://en.wikipedia.org/wiki/Degeneracy_(graph_theory)

kcore s []    = s
kcore s (c:q)
  | CS.notMember c s = kcore s q
  | length ns >= k   = kcore s q
  | otherwise        = kcore (CS.delete c s) (ns ++ q)
  where
    ns = live s c
