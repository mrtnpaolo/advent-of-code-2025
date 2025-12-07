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
  do inp <- getInputArray 7
     print (part1 inp)
     print (part2 inp)

part1 a = count ('^'==) [ a A.! c | c <- visited ]
  where
    (_,C yM _) = A.bounds a

    [s] = [ c | (c,'S') <- A.assocs a ]

    visited = bfs nexts s

    nexts c@(C y _)
      | y == yM   = []
      | otherwise = case a A.! c of
      'S' -> [below c]
      '.' -> [below c]
      '^' -> [left c,right c]

part2 a = arrivals s
  where
    (_,C yM _) = A.bounds a

    [s] = [ c | (c,'S') <- A.assocs a ]

    arrivals = memo $ \c@(C y _) ->
      if y == yM
        then 1
        else case a A.! c of
               'S' -> arrivals (below c)
               '^' -> arrivals (left c) + arrivals (right c)
               '.' -> arrivals (below c)
