module Super.Canvas.Trees ( BiTree (..)
                          , QualTree (..)
                          , BTContext (..)
                          , top
                          , qtUpMost
                          , sampleTree
                          , rotate
                          , prepTree ) where

import System.IO.Unsafe
import Control.Concurrent

import Control.Event.Handler (Handler)

import Super.Canvas.Types

confSize = 10
advCX = (+ (1.5 * confSize))
advCY = (+ (2.5 * confSize))

sampleTree i col = 
  if i > 0
     then BiNode (sampleTree (i - 1) (nextColor col)) col
                 (sampleTree (i - 1) ((nextColor . nextColor) col))
     else EmptyTree

data BiTree a = EmptyTree 
              | BiNode (BiTree a) a (BiTree a) 
              deriving (Show, Eq)

data BTContext a = Top
                 | L a (BTContext a) (BiTree a)
                 | R (BiTree a) a (BTContext a)
                 deriving (Show, Eq)

type QualTree a = (BiTree a, BTContext a)

qtLeft   (BiNode l v r, c)   = ( l, L v c r )
qtRight  (BiNode l v r, c)   = ( r, R l v c )

qtUp     (t, L v c r)        = (BiNode t v r, c)
qtUp     (t, R l v c)        = (BiNode l v t, c)

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top)     = (t, Top)

top :: BiTree a -> QualTree a
top t = (t, Top)

confSize' = (confSize, confSize)
confSize'' = (confSize * 2, confSize * 2)


prepTree :: Handler (BiTree Color) 
         -> Layout (BiTree Color)
prepTree f tree = 
  let rx = 450
      ry = 50
      (px,_) = compX 0 tree
      plate = trav f 0 ry (px,ry) (top tree)
  in translate (rx - px, 0) plate

trav :: (Handler (BiTree Color)) -> Double -> Double
     -> Location -> QualTree Color -> [Shape]
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
compX :: Double -> BiTree a -> (Double, Double)
compX x (BiNode EmptyTree _ EmptyTree) = 
  (x, advCX x) -- leaf node case
compX x EmptyTree = 
  (x, x) -- absent child case
compX x (BiNode l _ r) = 
  let (lx, x') = compX x l
      (rx, x'') = compX (advCX x') r 
  in (x', (x'')) 

sketchNode :: Color -> Location -> [Primitive]
sketchNode col ploc = [ Line (0,0) ploc 2
                      , Circle (0,0) confSize True col ]

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
