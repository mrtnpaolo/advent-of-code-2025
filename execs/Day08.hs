module Main (main) where

import Advent (getInputLines,pairs)
import Data.Ord (Down(..))
import Data.List          qualified as L
import Data.IntMap.Strict qualified as IM

main =
  do circuits <- connect <$> getInputLines (parse . map \case ','->' ';c->c) 8
     print (part1 circuits)
     print (part2 circuits)
  where
    parse = (\[x,y,z]->(x,y,z)) . map (read @Int) . words

part1 ((!! (1000 - 1)) -> (_,cs))
  = product . take 3 . L.sortOn Down . IM.elems
  . IM.fromListWith (+) $ [ (c,1) | c <- IM.elems cs ]

part2 (last -> (((xp,_,_),(xq,_,_)),_)) = xp * xq

connect points = go (IM.size m) IM.empty ps
  where
    hash (x,y,z) = 100*x + 10*y + z
    m = IM.fromList [ (hash p,p) | p <- points ]
    l = (m IM.!)

    dist (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
      where
        (dx,dy,dz) = (x1-x2,y1-y2,z1-z2)

    ps = [ (hash p,hash q) | (p,q) <- L.sortOn (uncurry dist) (pairs points) ]

    go 1 _ _ = []
    go n cs ((p,q):ps)
      | p'  <- find cs p
      , q'  <- find cs q
      , cs' <- IM.insert p' q' cs
      = ((l p,l q),cs') : if p' == q' then go n cs ps else go (n-1) cs' ps

    find cs p
      | p == p'   = p
      | otherwise = find cs p'
      where
        p' = IM.findWithDefault p p cs
