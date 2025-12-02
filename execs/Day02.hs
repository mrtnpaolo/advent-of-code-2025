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
  do inp <- getInput parse 2
     print (part1 inp)
     print (part2 inp)
  where
    parse = map (map (read @Int) . L.splitOn "-") . lines . map \case c | c `elem` "," -> '\n'; c -> c

part1 = sum . concatMap check

check [a,b] = [ n | n <- [a..b], invalid n ]

invalid n
  | odd l = False
  | otherwise = q == r
  where
    l = length (show n)
    (q,r) = n `divMod` (10^(l `div` 2))

part2 = sum . concatMap check2

check2 [a,b] = [ n | n <- [a..b], invalid2 n ]

invalid2 (show -> xs) =
  or [ same xss | n <- ns, let xss = L.splitPlaces (repeat n) xs ]
  where
    l = length xs
    ns = [1..l`div`2]

same (x:xs) = all (x==) xs
