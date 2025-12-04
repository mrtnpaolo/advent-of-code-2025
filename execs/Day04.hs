module Main (main) where

import Advent             (getInputArray, neighbors)
import Data.List          (unfoldr)
import Data.Array.Unboxed (assocs, (!), inRange, bounds, (//))
import Control.Monad      (guard)

main =
  do inp <- getInputArray 4
     print (part1 inp)
     print (part2 inp)

part1 = length . accessible

accessible a =
  [ c | (c,'@') <- assocs a
      , length [ n | n <- neighbors c, inRange (bounds a) n, a!n == '@' ] < 4 ]

part2 = sum . unfoldr \a ->
  do let cs = accessible a
     guard (not (null cs))
     pure (length cs, a // [ (c,'.') | c <- cs ])
