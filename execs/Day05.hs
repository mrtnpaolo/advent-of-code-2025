module Main (main) where

import Advent          (getInput, count)
import Data.Ix         (inRange)
import Data.List       (foldl', sort, findIndex)
import Data.List.Split (splitOn)

main =
  do (foldl' combine [] . sort -> rs,xs) <- getInput (parse . splitOn "\n\n") 5
     print (part1 rs xs)
     print (part2 rs)
  where
    parse [map range . lines -> rs, map int . lines -> xs] = (rs,xs)
    range = (\[a,b] -> (int a,int b)) . splitOn "-"
    int   = read @Int

part1 rs = count (\x -> any (`inRange` x) rs)

part2 rs = sum [ h-l+1 | (l,h) <- rs ]

combine rs (l,h) =
  case ( rs ? l , rs ? h ) of
    ( Just i  , Just j  ) | (xs,(a,_): _) <- splitAt i rs
                          , ( _,(_,b):ys) <- splitAt j rs -> (xs ++ (a,b) : ys)
    ( Just i  , Nothing ) | (xs,(a,_):ys) <- splitAt i rs -> (xs ++ (a,h) : ys)
    ( Nothing , Just j  ) | (xs,(_,b):ys) <- splitAt j rs -> (xs ++ (l,b) : ys)
    ( Nothing , Nothing )                                 -> (      (l,h) : rs)

rs ? x = findIndex (`inRange` x) rs
