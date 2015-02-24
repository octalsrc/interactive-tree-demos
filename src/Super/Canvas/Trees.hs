module Super.Canvas.Trees ( BiTree (..)
                          , QualTree (..)
                          , BTContext (..)
                          , top
                          , qtUpMost
                          , sampleTree
                          , rotate
                          , prepTree
                          , prepTree' ) where

import System.IO.Unsafe
import Control.Concurrent

import Control.Event.Handler (Handler)

import Super.Canvas.Types

data BiTree a = EmptyTree 
              | BiNode (BiTree a) a (BiTree a) 
              deriving (Show, Eq)

data BTContext a = Top
                 | L a (BTContext a) (BiTree a)
                 | R (BiTree a) a (BTContext a)
                 deriving (Show, Eq)

cval (L v _ _) = v
cval (R _ v _) = v

type QualTree a = (BiTree a, BTContext a)

qtLeft   (BiNode l v r, c)   = ( l, L v c r )
--qtLeft   (EmptyTree, c)      = (EmptyTree, c)
qtRight  (BiNode l v r, c)   = ( r, R l v c )
--qtRight  (EmptyTree, c)      = (EmptyTree, c)

qtUp     (t, L v c r)        = (BiNode t v r, c)
qtUp     (t, R l v c)        = (BiNode l v t, c)

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top)     = (t, Top)

top :: BiTree a -> QualTree a
top t = (t, Top)

confSize = 10

confSize' = (confSize, confSize)
confSize'' = (confSize * 2, confSize * 2)


prepTree' :: Handler (BiTree Color) -> Layout (BiTree Color)
prepTree' = prepTree

prepTree :: Handler (BiTree Color) 
         -> Layout (BiTree Color)
prepTree f tree = 
  let rx = 450
      ry = 50
      (px,_) = compX 0 tree
      plate = trav f 0 ry (px,ry) (top tree)
  in translate (rx - px, 0) plate


{-

trec :: Handler (BiTree Color) 
     -> QualTree Color
     -> (Double, Double) 
     -> (Double, Double) 
     -> Plate String
trec _ (EmptyTree, _   )   _   _  = []
trec f (BiNode l col r, c) pre sc = 
  let tree = (BiNode l col r, c)
      rotato = (fst . qtUpMost . rotate) tree
      loc = compLoc tree pre
      prevOffset = let (px,py) = pre
                       (lx,ly) = loc
                   in ( px - lx , py - ly )
      shape = Shape (show col)
                    confSize''
                    (tPrims col confSize prevOffset)
                    loc
                    [OnClick (return rotato >>= f)]
  in (trec f (qtLeft tree) loc sc) 
     ++ (trec f (qtRight tree) loc sc)
     ++ [(scale sc shape)]

trek f (lx,ty) tree =
  let (rx) = trek f (lx,(advC ly)) (qtLeft tree)
      (nx) = trek f (rx,(advC ly)) (qtRight tree)
      tx = 
      (BiNode _ col _) = fst tree
      rotato = (fst . qtUpMost . rotate) tree
      pd = parDims (tx,ty) (snd tree)
      shape = Shape (show col)
                    confSize''
                    (tPrims col confSize pd)
                    (tx,ty)
                    [OnClick (return rotato >>= f)] 
  in (nx, (lp ++ rp ++ [shape]))
trek f (lx,ly) (BiNode l col r, c) = undefined
        
-}

trav :: (Handler (BiTree Color)) -> Double -> Double
     -> Location -> QualTree Color -> [Shape]
--trav _ _ _ _ _ = [Shape (50,50) (50,50) [Circle (10,10) 10 True Red] []]
trav f lx y ploc (BiNode l col r, c) =
  let qt = (BiNode l col r, c)
      (x, _) = compX lx (fst qt)
      (_, rx') = compX lx l
      rx = advCX rx'
      y' = advCY y 
      loc = (x,y)
      (px,py) = ploc
      linedest = (px - x, py - y)
      rotato = (fst . qtUpMost . rotate) qt 
   in (trav f lx y' loc (qtLeft qt))
      ++ (trav f rx y' loc (qtRight qt)) 
      ++ [ Shape confSize'' 
                 loc 
                 (sketchNode col linedest)
                 [OnClick (return rotato >>= f)] ]

trav _ _ _ _ (EmptyTree, _) = []
        
{- Returns (x, x') such that 
     x  = the x coordinate of this node
     x' = the x coordinate of the next leaf node -}
compX' :: Double -> BiTree a -> (Double, Double)
--compX _ _ = (1,1)
compX' x (BiNode EmptyTree _ EmptyTree) = 
  (x, advCX x) -- leaf node case
compX' x EmptyTree = 
  (x, x) -- absent child case
compX' x (BiNode l _ r) = 
  let (lx, x') = compX' x l
      (rx, x'') = compX' x' r 
  in ((rx + lx) / 2, x'') 

compX :: Double -> BiTree a -> (Double, Double)
--compX _ _ = (1,1)
compX x (BiNode EmptyTree _ EmptyTree) = 
  (x, advCX x) -- leaf node case
compX x EmptyTree = 
  (x, x) -- absent child case
compX x (BiNode l _ r) = 
  let (lx, x') = compX x l
      (rx, x'') = compX (advCX x') r 
  in (x', (x'')) 

advCX = (+ (1.5 * confSize))
advCY = (+ (2.5 * confSize))

sketchNode :: Color -> Location -> [Primitive]
sketchNode col ploc = [ Line (0,0) ploc 2
                      , Circle (0,0) confSize True col ]

{- 

compLoc :: QualTree a -> (Double, Double) -> (Double, Double)
compLoc (t, L v c r) (x,y) = 
  let (nx,ny) = compLocH x y t r (-1)
  in ( nx , ny )
compLoc (t, R l v c) (x,y) =
  let (nx,ny) = compLocH x y t l (1)
  in ( nx , ny )
compLoc (t, Top) (x,y) = (x,y)

compLocH x y t o m = 
  ( x
    + m * 
     ( (confSize / 2) + confSize 
      * (fromIntegral ( (depth t) ^ (2 :: Int)
                       + (depth o) ^ (2 :: Int)) / 2))
  , y + confSize * 2 )

-}
depth :: BiTree a -> Int
depth EmptyTree = 0
depth (BiNode l _ r) = (max (depth l) (depth r)) + 1

countCh :: BiTree a -> Int
countCh (BiNode EmptyTree _ EmptyTree) = 1
countCh (BiNode l _ r) = (countCh l) + (countCh r)

countCh' :: QualTree a -> Int
countCh' = (countCh . fst . qtUpMost)

rotate :: QualTree a -> QualTree a
rotate ( (BiNode l v r), (L p c t) ) = 
  ( (BiNode l v (BiNode r p t)), c )
rotate ( (BiNode l v r), (R t p c) ) = 
  ( (BiNode (BiNode t p l) v r), c )
rotate ( tree, Top ) = ( tree, Top )

--  data BiTree a = BiTree { value :: a
--                         , leftT :: Maybe (BiTree a)
--                         , rightT :: Maybe (BiTree a) }

{- 
drawTree :: (Double, Double) -> Layout (BiTree ()) ()
drawTree (x,y) (BiTree v l r) (h,w) = 
     drawNode l (x,y) (x - 10 * depth l, y + 20) (h,w)
  ++ drawNode r (x,y) (x + 10 * depth l, y + 20) (h,w)
  ++ [ Shape v [Circle 10 True] (x,y) ]

depth :: Maybe (BiTree a) -> Double
depth (Just (BiTree _ l _)) = 2 * depth l
depth _ = 1
drawNode :: Maybe (BiTree ()) 
         -> (Double, Double) 
         -> (Double, Double)
         -> (Double, Double)
         -> [Shape ()]
drawNode (Just t) (a,b) (x,y) (h,w) = 
  [ Shape () [ Line a b 2] (x,y) ]
  ++ drawTree (x,y) t (h,w)
drawNode _ _ _ _ = []

sampleTree :: Int -> Maybe (BiTree ())
sampleTree n = 
  if n > 0
     then (Just (BiTree () (sampleTree (n - 1))
                           (sampleTree (n - 1))))
     else Nothing

-}

sampleTree i col = 
  if i > 0
     then BiNode (sampleTree (i - 1) (nextColor col)) col
                 (sampleTree (i - 1) ((nextColor . nextColor) col))
     else EmptyTree
