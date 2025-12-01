module Main (main) where

import Advent (getInputLines, count)

main =
  do (ps,zs) <- unzip . scanl click (0,50) <$> getInputLines parse 1
     print (part1 zs)
     print (part2 ps)
  where
    parse ('L':n) = negate (read @Int n)
    parse ('R':n) =        (read @Int n)

    click (_,n) k = (crossings, r)
      where
        (q,r) = (n + k) `divMod` 100
        crossings
          | k > 0     = abs q
          | otherwise = abs q   -- two problems arise when counting going left:
            + fromEnum (r == 0) -- land on zero, not counted by divMod, add one
            - fromEnum (n == 0) -- start from zero, counted extra, subtract one

part1 = count (0 ==)

part2 = sum
