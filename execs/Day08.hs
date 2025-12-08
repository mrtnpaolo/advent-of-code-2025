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

connect points = go 0 IM.empty ds
  where
    hash (x,y,z) = 100*x + 10*y + z
    m = IM.fromList [ (hash p,p) | p <- points ]
    l = (m IM.!)

    dist (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
      where
        (dx,dy,dz) = (x1-x2,y1-y2,z1-z2)

    ds = IM.fromListWith (++)
           [ (dist p' q',[(p,q)]) | ((p,p'),(q,q')) <- pairs (IM.assocs m) ]

    go n cs dm
      | IM.size cs == IM.size m && (\(x:xs) -> all (x==) xs) (IM.elems cs) = []
      | ((d,(p,q):rs),dm') <- IM.deleteFindMin dm
      , dm'' <- if null rs then dm' else IM.insert d rs dm'
      = let
          (n',cs') = case (cs IM.!? p,cs IM.!? q) of
            (Nothing,Nothing) -> (n+1,IM.insert p n $ IM.insert q n $ cs)
            (Just c1,Nothing) -> (n  ,IM.insert q c1 cs)
            (Nothing,Just c2) -> (n  ,IM.insert p c2 cs)
            (Just c1,Just c2)
              | c1 == c2      -> (n,cs)
              | True          -> (n,IM.map (\c -> if c==c2 then c1 else c) cs)
        in
          ((l p,l q),cs') : go n' cs' dm''


