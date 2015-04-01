module Super.Trees ( BiTree (..)
                   , QualTree (..)
                   , BTContext (..)
                   , randomTree
                   , top
                   , sampleHeapTree
                   , prepHeapTree
                   , qtUpMost 
                   , rotate
                   , prepTree
                   , prepSTree ) where

import Control.Event.Handler (Handler)
import System.Random
import qualified Data.List as L

import Super.Canvas

confSize = 10
distX = (1.5 * confSize)
distY = (2.5 * confSize)


randomTree :: [a] -> StdGen -> BiTree a
randomTree [] _ = EmptyTree
randomTree as g1 = let (root,g2) = randomR (0, length as - 1) g1
                       (g3,g4) = split g2
                       (l, (v : r)) = L.splitAt root as
                   in BiNode (randomTree l g3)
                             v
                             (randomTree r g4)

sampleHeapTree :: Int -> Int -> BiTree Int
sampleHeapTree i n = 
  if i > 0
     then BiNode (sampleHeapTree (i - 1) (n + 1)) n
                 (sampleHeapTree (i - 1) (n + 30))
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
qtUp     (t, Top    )        = (t, Top         )

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top)     = (t, Top)

top :: BiTree a -> QualTree a
top t = (t, Top)

confSize' = (confSize, confSize)
confSize'' = (confSize * 2, confSize * 2)

prepHeapTree :: Handler (BiTree Int)
             -> BiTree Int
             -> SuperForm
prepHeapTree f tree = travHeap f (top tree)

travHeap :: Handler (BiTree Int) 
         -> QualTree Int
         -> SuperForm
travHeap fire (BiNode l (intn) r, c) = 
  let qt = (BiNode l (intn) r, c)
      loc = findLoc qt
      linedest = findLoc (qtUp qt) - loc
      node = (sketchHeapNode 
                intn
                linedest
                [(return ((fst . qtUpMost . upheap) qt) >>= fire)]) 
  in combine 
       [ (travHeap fire (qtLeft qt))
       , (travHeap fire (qtRight qt))
       , translate loc node           ]
travHeap _ _ = blank

upheap :: QualTree Int -> QualTree Int
upheap (BiNode ll (intn) rr, (L v c r)) = (BiNode ll v rr, (L intn c r))
upheap (BiNode ll (intn) rr, (R l v c)) = (BiNode ll v rr, (R l intn c))
upheap (BiNode ll (intn) rr, Top) = (BiNode ll intn rr, Top)


prepTree :: Handler ( [(Vector, SuperForm)] 
                    , BiTree (Bool, Color)  ) 
         -> BiTree (Bool, Color) 
         -> SuperForm
prepTree f tree = trav f (top tree)

trav :: Handler ( [(Vector, SuperForm)]
                , BiTree (Bool, Color)  ) 
     -> QualTree (Bool, Color)
     -> SuperForm
trav fire (BiNode l (True,col) r, c) = 
  let qt = (BiNode l (True,col) r, c)
      loc = findLoc qt
      linedest = findLoc (qtUp qt) - loc
      node = (sketchNode 
                col 
                linedest
                [fire (rotatos qt)])
  in combine 
       [ (trav fire (qtLeft qt))
       , (trav fire (qtRight qt))
       , translate loc node       ]
trav _ _ = blank

prepSTree = prepTree (\_ -> return ())

prepRTree qt = let rloc = findLoc (qtUpMost qt)
                   tloc = findLoc (fst qt, Top)
               in translate ((0,0) - tloc) ((prepSTree (fst qt)))

mkInvis (BiNode l (_,col) r, c) = (BiNode l (False,col) r, c)
mkInvis t = t

rotatos :: QualTree (Bool, Color) 
        -> ( [(Vector, SuperForm)]
           , BiTree (Bool, Color)  )
rotatos (t, Top) = ([], t)
rotatos qt = ( [ rTopTree qt
               , rBottomTree qt
               , rUpTree qt
               , rDownTree qt ]
             , (fst . qtUpMost . rotate) qt )

qtCull qt = case qt of
              (_, L _ _ _) -> qtRight qt
              (_, R _ _ _) -> qtLeft qt

rUpTree qt = let rqt = rotate qt
             in ( findLoc rqt - findLoc qt
                , translate (findLoc qt) 
                            (prepRTree 
                               ((qtUp . mkInvis . qtCull) qt)) )

rDownTree qt = let rqt = rotate qt
                   desc = case qt of
                            (_, L _ _ _) -> qtRight
                            (_, R _ _ _) -> qtLeft
               in ( findLoc (desc rqt) - findLoc (qtUp qt)
                  , translate 
                      (findLoc (qtUp qt)) 
                      (prepRTree ((qtUp . mkInvis) qt)) )

rTopTree qt = ( (0,0)
              , prepSTree 
                  ((fst . qtUpMost . mkInvis . qtUp) qt) )

rBottomTree qt = ( (0,0)
                 , translate 
                     (findLoc (qtCull qt))
                     (prepRTree (qtCull qt)) )

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

sketchHeapNode :: Int -> Location -> [IO ()] -> SuperForm
sketchHeapNode col ploc acs = 
  let circ = rekt (idLocation - (confSize, confSize)) (confSize * 2, confSize * 2) Yellow
      tex = text (idLocation) (confSize * 2, confSize * 1.2) (show col)
  in combine [ line idLocation ploc 2
             , addOnClick 
                 acs 
                 circ 
             , tex ] 


sketchNode :: Color -> Location -> [IO ()] -> SuperForm
sketchNode col ploc acs = 
  let circ = circle idLocation confSize True col
  in combine [ line idLocation ploc 2
             , addOnClick 
                 acs 
                 circ ] 

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
