module Main (main) where

import Advent    (getInputLines)
import Data.Ord  (comparing)
import Data.List (maximumBy, tails)
import Data.Char (digitToInt, intToDigit)

main =
  do inp <- getInputLines (map digitToInt) 3
     print (part1 inp)
     print (part2 inp)

part1 = score . map (pick 2)

part2 = score . map (pick 12)

pick 0 _  = []
pick n xs = x : pick (n-1) best
  where
    viable   = take (length xs - n + 1) (tails xs)
    (x:best) = maximumBy (comparing head <> comparing length) viable

score = sum . map (read @Int . map intToDigit)
