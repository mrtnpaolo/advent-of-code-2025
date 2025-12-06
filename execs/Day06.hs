module Main (main) where

import Advent          (getInput)
import Data.List       (transpose)
import Data.List.Split (splitWhen)

main =
  do inp <- getInput (splitWhen (all (' ' ==)) . transpose . lines) 6
     print (part1 inp)
     print (part2 inp)

part1 = sum . map ((\xs    -> w (head (last xs)) (init xs)) . transpose)

part2 = sum . map (\(x:xs) -> w (last x) (init x : xs))

w '+' =     sum . map read
w '*' = product . map read
