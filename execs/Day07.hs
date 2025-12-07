module Main (main) where

import Advent.Input  (getInputArray)
import Advent.List   (count)
import Advent.Memo   (memo)
import Advent.Coord  (Coord(..),coordRow,left,below,right)
import Advent.Search (bfs)

import Data.Array.Unboxed qualified as A

main =
  do inp <- getInputArray 7
     print (part1 inp)
     print (part2 inp)

part1 a = count ('^'==) [ a A.! c | c <- bfs nexts (start a) ]
  where
    nexts c | end a c   = []
            | otherwise = case a A.! c of
                '.' -> [below c]
                '^' -> [left c,right c]

part2 a = arrivals (start a)
  where
    arrivals = memo $
      \case c | end a c   -> 1
              | otherwise ->
              case a A.! c of
                '.' -> arrivals (below c)
                '^' -> arrivals (left  c) + arrivals (right c)

start a = below c where [c] = [ c | (c,'S') <- A.assocs a ]

end a (C y _) = y == yM where (_,C yM _) = A.bounds a
