module Super.Canvas.Trees ( BiTree (..)
                          , sampleTree
                          , drawTree ) where

import Super.Canvas.Types

data BiTree a = BiTree { value :: a
                       , leftT :: Maybe (BiTree a)
                       , rightT :: Maybe (BiTree a) }

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
