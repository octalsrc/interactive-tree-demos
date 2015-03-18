module Super.Canvas.Trees ( BiTree (..)
                          , QualTree (..)
                          , BTContext (..)
                          , top
                          , qtUpMost
                          , sampleTree
                          , randomTrees
                          , rotate
                          , prepTree
                          , prepSTree ) where

-- import Control.Concurrent

import Control.Event.Handler (Handler)
import System.Random

-- import Super.Canvas.JS
import Super.Canvas.Types

confSize = 10
distX = (1.5 * confSize)
distY = (2.5 * confSize)

randomTrees :: Int -> Int -> (BiTree (Bool, Color), BiTree (Bool, Color))
randomTrees i r = let g = mkStdGen r
                      (col,g') = (random g)
                      bs = (randoms g) :: [Bool]
                      bs' = (randoms g') :: [Bool]
                      tree = sampleTree i col 
                  in ( (fst . qtUpMost . fst) (walk (top tree) bs)
                     , (fst . qtUpMost . fst) (walk (top tree) bs') )

walk (EmptyTree, c) (b:bs) = ((EmptyTree, c), bs)
walk (BiNode l v r, c) (a:b:bs) = 
  let tree = (BiNode l v r,c)
      newr = if b
                then (fst . qtUpMost . fst) (walk (r,Top) bs)
                else r
      newtree = (BiNode l v newr,c)
      ltree = if a
                 then (qtLeft . rotate) newtree
                 else qtLeft newtree
  in walk (ltree) bs

sampleTree i col = 
  if i > 0
     then BiNode (sampleTree (i - 1) (nextColor col)) (True, col)
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

type TreeShift = ([Traveller], BiTree (Bool, Color))

qtLeft   (BiNode l v r, c)   = ( l, L v c r )
qtRight  (BiNode l v r, c)   = ( r, R l v c )

qtUp     (t, L v c r)        = (BiNode t v r, c)
qtUp     (t, R l v c)        = (BiNode l v t, c)
qtUp     (t, Top    )        = (t, Top         )

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top)     = (t, Top)

top :: BiTree a -> QualTree a
top t = (t, Top)

confSize' = (confSize, confSize)
confSize'' = (confSize * 2, confSize * 2)

prepTree :: Handler TreeShift
         -> Layout (BiTree (Bool, Color))
prepTree f tree = trav f (top tree)

trav :: Handler TreeShift 
     -> QualTree (Bool, Color)
     -> [Shape]
trav fire (BiNode l (True,col) r, c) = 
  let qt = (BiNode l (True,col) r, c)
      loc = findLoc qt
      linedest = findLoc (qtUp qt) - loc
  in (trav fire (qtLeft qt))
     ++ (trav fire (qtRight qt))
     ++ [ Shape confSize''
                loc
                (sketchNode col linedest)
                [OnClick (return (rotatos qt) >>= fire)] ]
trav _ _ = []

prepSTree :: Layout (BiTree (Bool, Color))
prepSTree = prepTree (\_ -> return ())

prepRTree qt = let rloc = findLoc (qtUpMost qt)
                   tloc = findLoc (fst qt, Top)
               in translate ((0,0) - tloc) ((prepSTree (fst qt)))

mkInvis (BiNode l (_,col) r, c) = (BiNode l (False,col) r, c)
mkInvis t = t

rotatos' qt = ([], fst . qtUpMost . rotate $ qt)

rotatos :: QualTree (Bool, Color)
        -> TreeShift
rotatos (t, Top) = ([], t)
rotatos qt = ( [ rTopTree qt
               , rBottomTree qt
               , rUpTree qt
               , rDownTree qt ]
             , (fst . qtUpMost . rotate) qt )
  where qtcull = case qt of
                   (_, L _ _ _) -> qtRight
                   (_, R _ _ _) -> qtLeft

qtCull qt = case qt of
              (_, L _ _ _) -> qtRight qt
              (_, R _ _ _) -> qtLeft qt

rUpTree qt = let rqt = rotate qt
             in ( prepRTree ((qtUp . mkInvis . qtCull) qt)
                , findLoc qt, findLoc rqt)

rDownTree qt = let rqt = rotate qt
                   desc = case qt of
                            (_, L _ _ _) -> qtRight
                            (_, R _ _ _) -> qtLeft
               in ( prepRTree ((qtUp . mkInvis) qt)
                  , findLoc (qtUp qt), findLoc (desc rqt))

rTopTree qt = ( prepSTree ((fst . qtUpMost . mkInvis . qtUp) qt)
              , (0,0), (0,0))

rBottomTree qt = ( prepRTree (qtCull qt)
                 , findLoc (qtCull qt)
                 , findLoc (qtCull qt) )

findLoc :: QualTree a -> Location
findLoc tree = (tx (findX tree), ty (depthOf tree - 1))
  where tx = (*) distX . fromIntegral
        ty = (*) distY . fromIntegral

findX :: QualTree a -> Int
findX qt = case qt of
             (BiNode l _ r, Top) -> 
               let lsubtree = (fst . qtLeft) qt
                   initXIndex = 0
               in nextX initXIndex lsubtree
             (BiNode l _ r, L _ _ _) -> 
               let rsubtree = (fst . qtRight) qt
                   initXIndex = 0
               in findX (qtUp qt) - nextX initXIndex rsubtree - 1
             (BiNode l _ r, R _ _ _) -> 
               let lsubtree = (fst . qtLeft) qt
                   initXIndex = 0
               in findX (qtUp qt) + nextX initXIndex lsubtree + 1
  where nextX :: Int -> BiTree a -> Int
        nextX x (BiNode EmptyTree _ EmptyTree) =
          x + 1 -- leaf node case
        nextX x EmptyTree =
          x -- absent child case
        nextX x (BiNode l _ r) =
          let x' = nextX x l 
          in nextX (x' + 1) r

sketchNode :: Color -> Location -> [Primitive]
sketchNode col ploc = [ Line ploc 2
                      , Circle confSize True col ]

depth :: BiTree a -> Int
depth EmptyTree = 0
depth (BiNode l _ r) = (max (depth l) (depth r)) + 1

depthOf :: QualTree a -> Int
depthOf (_, Top) = 1
depthOf t = ((+1) . depthOf . qtUp) t

rotate :: QualTree a -> QualTree a
rotate ( (BiNode l v r), (L p c t) ) = 
  ( (BiNode l v (BiNode r p t)), c )
rotate ( (BiNode l v r), (R t p c) ) = 
  ( (BiNode (BiNode t p l) v r), c )
rotate ( tree, Top ) = ( tree, Top )
