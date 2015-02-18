module Super.Canvas.Trees ( BiTree (..)
                          , sampleTree
                          , prepTree
                          , prepTree' ) where

import Control.Event.Handler (Handler)

import Super.Canvas.Types

data BiTree a = EmptyTree 
              | BiNode (BiTree a) a (BiTree a) deriving Show

data BTContext a = Top
                 | L a (BTContext a) (BiTree a)
                 | R (BiTree a) a (BTContext a)

cval (L v _ _) = v
cval (R _ v _) = v

type QualTree a = (BiTree a, BTContext a)

qtLeft   (BiNode l v r, c)   = ( l, L v c r )
qtRight  (BiNode l v r, c)   = ( r, R l v c )

qtUp     (t, L v c r)        = (BiNode t v r, c)
qtUp     (t, R l v c)        = (BiNode l v t, c)

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top) = (t, Top)

top :: BiTree a -> QualTree a
top      t                   = (t, Top)

confSize = 10

confSize' = (confSize, confSize)


prepTree' :: Handler (BiTree Int) -> Layout (BiTree Int) String
prepTree' = prepTree

prepTree :: Show a 
         => Handler (BiTree a) 
         -> Layout (BiTree a) String
prepTree f tree = trec f (top tree) (0,0)

trec :: Show a
     => Handler (BiTree a) 
     -> QualTree a 
     -> (Double, Double)
     -> (Double, Double)
     -> Plate String
trec _ (EmptyTree, _   ) _   _  = []
trec f (BiNode l v r, c) pre sc = 
  let tree = (BiNode l v r, c)
      loc = compLoc tree pre
      prevOffset = let (px,py) = pre
                       (lx,ly) = loc
                   in ( lx - px , ly - py )
      shape = Shape (show v)
                    confSize'
                    (tPrims confSize prevOffset)
                    loc
                    [OnClick (return (fst (rotate tree)) >>= f)]
  in (trec f (qtLeft tree) loc sc) 
     ++ (trec f (qtRight tree) loc sc)
     ++ [(scale sc shape)]

tPrims :: Double -> (Double, Double) -> [Primitive]
tPrims cs pr = [ Line (0,0) pr 2
               , Circle (0,0) cs True
               ]


compLoc :: QualTree a -> (Double, Double) -> (Double, Double)
compLoc (t, L v c r) (x,y) = 
  let (nx,ny) = compLocH x y t r
  in ( (-1) * nx , ny )
compLoc (t, R l v c) (x,y) =
  let (nx,ny) = compLocH x y t l
  in ( nx , ny )
compLoc (t, Top) (x,y) = (x,y)

compLocH x y t o = 
  ( y + confSize
  , confSize * (fromIntegral ( (depth t) ^ (2 :: Int)
                               + (depth o) ^ (2 :: Int))) )

depth :: BiTree a -> Int
depth EmptyTree = 0
depth (BiNode l _ r) = (max (depth l) (depth r)) + 1

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

sampleTree i = if i > 0
                  then BiNode (sampleTree (i - 1)) 5
                              (sampleTree (i - 1))
                  else EmptyTree
