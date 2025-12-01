module Main (main) where

import Advent (getInputLines, count)

main =
  do inp <- scanl clicks [50] <$> getInputLines parse 1
     print (part1 inp)
     print (part2 inp)
  where
    parse ('L':n) = negate (read @Int n)
    parse ('R':n) =        (read @Int n)

clicks (last -> n) k = take (abs k) . tail . iterate click $ n
  where
    click = (`mod` 100) . (+ (signum k))

part1 = count (0 ==) . map last

part2 = count (0 ==) . concat
