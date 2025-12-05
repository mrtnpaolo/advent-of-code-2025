module Main (main) where

import Advent          (getInput, count)
import Data.Ix         (inRange)
import Data.List       (foldl', sort)
import Data.List.Split (splitOn)

main =
  do (rs,xs) <- getInput (parse . splitOn "\n\n") 5
     print (part1 rs xs)
     print (part2 rs)
  where
    parse [map range . lines -> rs, map int . lines -> xs] = (rs,xs)
    range = (\[a,b] -> (int a,int b)) . splitOn "-"
    int   = read @Int

part1 rs = count (\x -> any (`inRange` x) rs)

part2 = fst . foldl' go (0,0) . sort
  where
    go (n,x) (l,h)
      | x >  h    = (n          ,x  )
      | x <= l    = (n + (h-l+1),h+1)
      | otherwise = (n + (h-x+1),h+1)
