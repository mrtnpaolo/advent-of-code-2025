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
import Control.Monad

main =
  do inp <- getInput parse 5
     print (part1 inp)
     print (part2 inp)
  where
    parse (L.splitOn "\n\n" -> [rs,xs]) = (rs',xs')
      where
        xs' = map (read @Int) . lines $ xs
        rs' = map (map (read @Int) . L.splitOn "-") . lines $ rs

part1 (rs,xs) = count (fresh rs) xs

fresh rs x = any f rs
  where
    f [lo,hi] = lo <= x && x <= hi

-- part2 (rs,_) = rs -- length $ concat $ [ [lo..hi] | [lo,hi] <- rs ]

part2 (rs,_) = total $ L.foldl' combine [] (L.sort rs)

total rs = sum [ h-l+1 | [l,h] <- rs ]

combine rs x@[l,h]
  | Just i  <- f rs l, Just j  <- f rs h
  , (xs,[a,_]: _) <- splitAt i rs
  , ( _,[_,d]:ys) <- splitAt j rs        = L.sort (xs ++ [a,d] : ys)

  | Just i  <- f rs l, Nothing <- f rs h
  , (xs,[a,b]:ys) <- splitAt i rs        = L.sort (xs ++ [a,h] : ys)

  | Nothing <- f rs l, Just j  <- f rs h
  , (xs,[a,b]:ys) <- splitAt j rs        = L.sort (xs ++ [l,b] : ys)

  | Nothing <- f rs l, Nothing <- f rs h = L.sort (x : rs)

f rs x = L.findIndex (\[a,b] -> a <= x && x <= b) rs
