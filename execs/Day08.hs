module Main (main) where

import Advent (getInputLines)
import Data.Ord (Down(..))
import Data.List          qualified as L
import Data.IntMap.Strict qualified as IM

main =
  do circuits <- connect <$> getInputLines (parse . map \case ','->' ';c->c) 8
     print (part1 circuits)
     print (part2 circuits)
  where
    parse = (\[x,y,z]->(x,y,z)) . map (read @Int) . words

part1 ((!! (1000 - 1)) -> (_,sizes)) = product . take 3 . L.sortOn Down $ sizes

part2 (last -> (((xp,_,_),(xq,_,_)),_)) = xp * xq

connect points = go (length sorted) IM.empty pairs
  where
    sorted = L.sortOn (\(x,_,_) -> x) points

    hash (x,y,z) = 100*x + 10*y + z

    dist (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
      where (dx,dy,dz) = (x1-x2,y1-y2,z1-z2)

    pairs        = concat [ inside lo hi | lo <- 0:bands | hi <- bands ]
    bands        = [20000,22500..]
    inside lo hi = L.sortOn (uncurry dist)
      [ (p,q) | p@(px,_,_) : qs <- L.tails sorted
              , q <- takeWhile (\(qx,_,_) -> qx < px + hi) qs
              , let d = dist p q, lo*lo <= d, d < hi*hi ]

    go 1 _ _ = []
    go n cs ((p,q):rs)
      | rp == rq  = ((p,q),sizes cs ) : go n     cs  rs
      | otherwise = ((p,q),sizes cs') : go (n-1) cs' rs
      where
        rp  = find cs (hash p)
        rq  = find cs (hash q)
        cs' = IM.insert rp rq cs

    sizes cs = IM.elems $
      IM.fromListWith (+) [ (find cs (hash p),1) | p <- sorted ]

    find cs p
      | p == rp   = p
      | otherwise = find cs rp
      where rp = IM.findWithDefault p p cs
