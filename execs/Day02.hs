module Main (main) where

import Advent          (getInput)
import Data.Set        (fromList)
import Data.List.Split (splitOn)

main =
  do inp <- getInput parse 2
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (map (read @Int) . splitOn "-") . splitOn ","

part1 = sum . ids True

part2 = sum . ids False

ids twice ranges = fromList
  [ p * b                                             -- pattern * base

  | let n = length . show . maximum . concat $ ranges -- max digits

  , [lo, hi] <- ranges                                -- for each range

  , k <- [1..n `div` 2]                               -- base sequence length

  , r <- if twice then [2] else [2..n `div` k]        -- repetition count

  -- for the pattern we place a 1 every k digits r times in total
  --
  --      k=1    k=2        k=3            k=4
  -- r=2  11     101        1001           10001
  -- r=3  111    10101      1001001        100010001
  -- r=4  1111   1010101    1001001001     1000100010001
  -- r=5  11111  101010101  1001001001001  ...
  --
  -- that is 10^0 + 10^k + 10^(2k) + 10^(3k) + ... + 10^((r-1)k)
  --
  -- by the geometric series (10^(rk) - 1)/(10^k - 1)

  , let p = (10^(r*k) - 1) `div` (10^k - 1)           -- repeating pattern

  -- the relevant base sequences are  b ∈ [10^(k-1),10^k - 1]  such that
  -- lo <= p * b <= hi  which means  lo/p <= b <= hi/p

  , let bm = max (10^(k-1)) ((lo + p - 1) `div` p)    -- integral ceil (a+b-1)/b
  , let bM = min (10^k - 1) (hi           `div` p)    -- already floors

  , b <- [bm..bM] ]                                   -- base sequences
