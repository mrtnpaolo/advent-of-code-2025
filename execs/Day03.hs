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
  do inp <- getInputLines parse 3
     print (part1 inp)
     print (part2 inp)
  where
    parse = map digitToInt

part1 = sum . map maxjoltage

maxjoltage xs = maximum [ a*10+b | (a,b) <- pairs xs ]

part2 = sum . map (read @Int . map intToDigit . twelve)

twelve xs = head [ reverse ds | (ds,12,_,_) <- bfsOn repr next [start] ]
  where
    start = ([],0,xs,length xs)

    repr (ds,_,rs,_) = (ds,rs)

    next (ds,n,rs,r) -- (picked, length picked, remaining, length remaining)
      | n < 12
      , rss <- takeWhile ((>= (12-n)) . length) . L.tails $ rs
      , ((_,x:xs):_) <- L.sortOn fst [ (-x,xs) | xs@(x:_) <- rss ]
      = [ (x:ds,n+1,xs,r-1) ]
      | otherwise = []
