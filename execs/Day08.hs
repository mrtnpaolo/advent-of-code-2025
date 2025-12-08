module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Bits
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
  do inp <- getInputLines parse 8
     print (part1 inp)
     print (part2 inp)
  where
    parse = (\[x,y,z]->(x,y,z)) . map (read @Int) . words
          . map \case ','->' ';c->c

part1 ps = go 0 IM.empty ds'
  where
    m = IM.fromList [ (h p,p) | p <- ps ]
    l = (m IM.!)
    ds = IM.fromListWith (++)
           [ (d (l p) (l q),[(p,q)]) | (p,q) <- pairs (IM.keys m) ]
    ds' = IM.fromAscListWith (++) $ take 1000 $
           [ (d,[pq]) | (d,pqs) <- IM.toAscList ds, pq <- pqs ]

    go n cs dm

      | IM.null dm = product . take 3 . reverse . L.sort
                   . map length . L.group . L.sort . IM.elems $ cs

      | ((d,(p,q):rs),dm') <- IM.deleteFindMin dm
      , dm'' <- if null rs then dm' else IM.insert d rs dm' =

        case (cs IM.!? p,cs IM.!? q) of

          (Nothing,Nothing) -> go (n+1) cs' dm''
            where
              cs' = IM.insert p n $ IM.insert q n $ cs

          (Just a ,Nothing) -> go n cs' dm'' where cs' = IM.insert q a cs

          (Nothing,Just b ) -> go n cs' dm'' where cs' = IM.insert p b cs

          (Just a ,Just b )
            | a == b    -> go n cs dm''
            | otherwise -> go n cs' dm''
            where
              k = min a b
              k' = max a b
              cs' = traceShow (a,b,k,k') (IM.map (const k') $ IM.filter (k==) cs) `IM.union` cs


part2 = const ()

h (x,y,z) = 100*x + 10*y + z

d :: (Int,Int,Int) -> (Int,Int,Int) -> Int
d (x1,y1,z1) (x2,y2,z2) = floor (sqrt (d x1 x2 + d y1 y2 + d z1 z2))
  where
    d a b = fromIntegral $ (a - b)*(a - b)
