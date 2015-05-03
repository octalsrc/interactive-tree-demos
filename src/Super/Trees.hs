module Super.Trees ( BiTree (..)
                   , QualTree (..)
                   , BTContext (..)
                   , randomTree
                   , makeHeap
                   , heapInsert
                   , heapCarelessInsert
                   , randomChild
                   , top
                   , sampleHeapTree
                   , prepHeapTree
                   , rotate
                   , prepTree
                   , prepSTree
                   , TreeR (..)
                   , ColorTree (..)
                   , qtUp
                   , qtLeft
                   , qtRight
                   , qtUpMost
                   , qtCut
                   , qtReplace

                   , ZTree (..)
                   , zTop
                   , ztUp
                   , ztLeft
                   , ztRight
                   , ztUpMost
                   , ztCut
                   , ztReplace

                   , depth
                   , leaf ) where

import Control.Event.Handler (Handler)
import System.Random
import qualified Data.List as L

import Super.Canvas

data TreeR = TreeR { trTree :: ColorTree
                   , trForm :: SuperForm }

type ColorTree = BiTree (Bool, Color)

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

heapInsert :: Ord a => a -> BiTree a -> BiTree a
heapInsert v EmptyTree = BiNode EmptyTree v EmptyTree
heapInsert v (BiNode l n r) = 
  if v > n
     then heapInsert n (BiNode l v r)
     else if shallow l > shallow r
             then BiNode l n (heapInsert v r)
             else BiNode (heapInsert v l) n r

heapCarelessInsert :: Ord a => a -> BiTree a -> BiTree a
heapCarelessInsert v EmptyTree = BiNode EmptyTree v EmptyTree
heapCarelessInsert v (BiNode l n r) = 
  if shallow l > shallow r
     then BiNode l n (heapCarelessInsert v r)
     else BiNode (heapCarelessInsert v l) n r 


makeHeap :: Ord a => [a] -> BiTree a
makeHeap = foldr heapInsert EmptyTree

makeHeap' :: Ord a => [a] -> BiTree a
makeHeap' [] = EmptyTree
makeHeap' as = let root = indexG 0 (head as) 0 as
                   (l, (v : r)) = L.splitAt root as
               in BiNode (makeHeap' l)
                         v
                         (makeHeap' r)

indexG :: Ord a => Int -> a -> Int -> [a] -> Int
indexG i gv gi [] = gi
indexG i gv gi (a:as) = if a > gv
                           then indexG (i+1) a i as
                           else indexG (i+1) gv gi as

randomChild :: StdGen -> QualTree a -> QualTree a
randomChild _ (EmptyTree,c) = (EmptyTree,c)
randomChild g t = let (dir,ng) = random g
                  in if dir
                        then randomChild ng (qtLeft t)
                        else randomChild ng (qtRight t)

sampleHeapTree :: Int -> Int -> BiTree Int
sampleHeapTree i n = 
  if i > 0
     then BiNode (sampleHeapTree (i - 1) (n + 1)) n
                 (sampleHeapTree (i - 1) (n + 30))
     else EmptyTree

data BiTree a = EmptyTree 
              | BiNode { btLeft :: BiTree a
                       , btVal  :: a
                       , btRight :: BiTree a } -- (BiTree a) a (BiTree a) 
              deriving (Show, Eq)

instance Functor BiTree where
  fmap f EmptyTree = EmptyTree
  fmap f (BiNode l v r) = BiNode (fmap f l) (f v) (fmap f r)

leaf :: a -> BiTree a
leaf a = BiNode EmptyTree a EmptyTree

data BTContext a = Top
                 | L a (BTContext a) (BiTree a)
                 | R (BiTree a) a (BTContext a)
                 deriving (Show, Eq)

instance Functor BTContext where
  fmap f Top = Top
  fmap f (L v c r) = L (f v) (fmap f c) (fmap f r)
  fmap f (R l v c) = R (fmap f l) (f v) (fmap f c)

type QualTree a = (BiTree a, BTContext a)

data ZTree a = ZTree { zTree :: (BiTree a)
                     , zContext :: (BTContext a) }

instance Functor ZTree where
  fmap f (ZTree t c) = ZTree (fmap f t) (fmap f c)

ztLeft   (ZTree (BiNode l v r) c)   = ZTree l (L v c r)
ztLeft   zt                         = zt
ztRight  (ZTree (BiNode l v r) c)   = ZTree r (R l v c)
ztRight  zt                         = zt

qtLeft   (BiNode l v r, c)   = ( l, L v c r )
qtRight  (BiNode l v r, c)   = ( r, R l v c )

ztUp     (ZTree t (L v c r))        = ZTree (BiNode t v r) c
ztUp     (ZTree t (R l v c))        = ZTree (BiNode l v t) c
ztUp     (ZTree t Top      )        = ZTree t Top

qtUp     (t, L v c r)        = (BiNode t v r, c)
qtUp     (t, R l v c)        = (BiNode l v t, c)
qtUp     (t, Top    )        = (t, Top         )

ztUpMost :: ZTree a -> ZTree a
ztUpMost (ZTree t (L v c r)) = ztUpMost (ZTree (BiNode t v r) c)
ztUpMost (ZTree t (R l v c)) = ztUpMost (ZTree (BiNode l v t) c)
ztUpMost (ZTree t Top)       = ZTree t Top

qtUpMost :: QualTree a -> QualTree a
qtUpMost (t, L v c r) = qtUpMost (BiNode t v r, c)
qtUpMost (t, R l v c) = qtUpMost (BiNode l v t, c)
qtUpMost (t, Top)     = (t, Top)

ztReplace :: a -> ZTree a -> ZTree a
ztReplace v (ZTree (BiNode l _ r) c) = ZTree (BiNode l v r) c
ztReplace v (ZTree EmptyTree c) = ZTree (leaf v) c

qtReplace :: a -> QualTree a -> QualTree a
qtReplace v (BiNode l _ r,c) = (BiNode l v r,c)
qtReplace _ qt = qt

ztCut :: ZTree a -> ZTree a
ztCut (ZTree _ (L v c r)) = ZTree (BiNode EmptyTree v r) c
ztCut (ZTree _ (R l v c)) = ZTree (BiNode l v EmptyTree) c
ztCut (ZTree _ Top)       = ZTree EmptyTree Top

qtCut :: QualTree a -> QualTree a
qtCut (_,L v c r) = (BiNode EmptyTree v r,c)
qtCut (_,R l v c) = (BiNode l v EmptyTree,c)
qtCut (_,Top) = (EmptyTree,Top)

zTop :: BiTree a -> ZTree a
zTop t = ZTree t Top

top :: BiTree a -> QualTree a
top t = (t, Top)

confSize' = (confSize, confSize)
confSize'' = (confSize * 2, confSize * 2)

prepHeapTree :: Handler (BiTree (Int,Bool))
             -> BiTree (Int,Bool)
             -> SuperForm
prepHeapTree f tree = travHeap f (top tree)

travHeap :: Handler (BiTree (Int,Bool)) 
         -> QualTree (Int,Bool)
         -> SuperForm
travHeap fire (BiNode l (intn,bl) r, c) = 
  let qt = (BiNode l (intn,bl) r, c)
      loc = findLoc qt
      linedest = findLoc (qtUp qt) - loc
      node = (sketchHeapNode 
                bl
                intn
                linedest
                [(return ((fst . qtUpMost . upheap) qt) >>= fire)]) 
  in combine 
       [ (travHeap fire (qtLeft qt))
       , (travHeap fire (qtRight qt))
       , translate loc node           ]
travHeap _ _ = blank

upheap :: QualTree a -> QualTree a
upheap (BiNode ll (intn) rr, (L v c r)) = (BiNode ll v rr, (L intn c r))
upheap (BiNode ll (intn) rr, (R l v c)) = (BiNode ll v rr, (R l intn c))
upheap (BiNode ll (intn) rr, Top) = (BiNode ll intn rr, Top)


prepTree :: Handler TreeR 
         -> Color
         -> BiTree (Bool, Color) 
         -> SuperForm
prepTree f lcol tree = trav f lcol (top tree)

trav :: Handler TreeR 
     -> Color
     -> QualTree (Bool, Color)
     -> SuperForm
trav fire lcol (BiNode l (True,col) r, c) = 
  let qt = (BiNode l (True,col) r, c)
      loc = findLoc qt
      linedest = findLoc (qtUp qt) - loc
      node = (sketchNode 
                lcol
                col 
                linedest
                [fire (rotatos qt)])
  in combine 
       [ (trav fire lcol (qtLeft qt))
       , (trav fire lcol (qtRight qt))
       , translate loc node       ]
trav _ _ _ = blank

prepSTree = prepTree (\_ -> return ())

prepRTree qt = let rloc = findLoc (qtUpMost qt)
                   tloc = findLoc (fst qt, Top)
               in translate ((0,0) - tloc) ((prepSTree Gray (fst qt)))

mkInvis (BiNode l (_,col) r, c) = (BiNode l (False,col) r, c)
mkInvis t = t

rotatos :: QualTree (Bool, Color) 
        -> TreeR
rotatos (t, Top) = TreeR t (prepSTree Gray t)
rotatos qt = TreeR ((fst . qtUpMost . rotate) qt)
                   (combine [ rTopTree qt
                            , rBottomTree qt
                            , rUpTree qt
                            , rDownTree qt   ])

qtCull qt = case qt of
              (_, L _ _ _) -> qtRight qt
              (_, R _ _ _) -> qtLeft qt

rUpTree qt = let rqt = rotate qt
                 loc = findLoc qt
                 vect = findLoc rqt - loc
                 tree = prepRTree ((qtUp . mkInvis . qtCull) qt)
             in (travel vect . translate loc) tree

rDownTree qt = let rqt = rotate qt
                   loc = findLoc (qtUp qt) 
                   desc = case qt of
                            (_, L _ _ _) -> qtRight
                            (_, R _ _ _) -> qtLeft
                   vect = findLoc (desc rqt) - loc
                   tree = prepRTree ((qtUp . mkInvis) qt)
               in (travel vect . translate loc) tree

rTopTree qt = prepSTree Gray ((fst . qtUpMost . mkInvis . qtUp) qt)

rBottomTree qt = let loc = findLoc (qtCull qt)
                 in translate loc (prepRTree (qtCull qt))

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

sketchHeapNode :: Bool -> Int -> Location -> [IO ()] -> SuperForm
sketchHeapNode bl col ploc acs = 
  let rcolor = if bl
                  then Yellow
                  else White
      circ = rekt (idLocation - (confSize, confSize)) (confSize * 2, confSize * 2) True rcolor
      tex = text (idLocation) (confSize * 2, confSize * 1.2) (show col)
  in if bl
        then combine [ line idLocation ploc 2 Black
                     , addOnClick 
                         acs 
                         circ 
                     , tex ] 
        else combine [ line idLocation ploc 2 Black
                     , circ
                     , tex ]


sketchNode :: Color -> Color -> Location -> [IO ()] -> SuperForm
sketchNode lcol col ploc acs = 
  let circ = circle idLocation confSize True col
  in combine [ line idLocation ploc 2 lcol
             , addOnClick 
                 acs 
                 circ ] 

depth :: BiTree a -> Int
depth EmptyTree = 0
depth (BiNode l _ r) = (max (depth l) (depth r)) + 1

shallow :: BiTree a -> Int
shallow EmptyTree = 0
shallow (BiNode l _ r) = (min (shallow l) (shallow r)) + 1

depthOf :: QualTree a -> Int
depthOf (_, Top) = 1
depthOf t = ((+1) . depthOf . qtUp) t

rotate :: QualTree a -> QualTree a
rotate ( (BiNode l v r), (L p c t) ) = 
  ( (BiNode l v (BiNode r p t)), c )
rotate ( (BiNode l v r), (R t p c) ) = 
  ( (BiNode (BiNode t p l) v r), c )
rotate ( tree, Top ) = ( tree, Top )

