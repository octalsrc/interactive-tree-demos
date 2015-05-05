module Super.Trees2 where

import Super.Canvas
import Super.Trees

data EditTree a = EditTree { etTree :: (ZTree a) } deriving Show

data Ord a => Heap a = Heap { hTree :: (BiTree a) } deriving Show

newHeap :: Ord a => Heap a
newHeap = Heap EmptyTree

insert :: Ord a => Bool -> a -> Heap a -> Heap a
insert b a (Heap t) = Heap (i a t)
  where i a (BiNode l v r) = 
          if compare' a v
             then i v (BiNode l a r)
             else if shallow l > shallow r
                     then BiNode l v (i a r)
                     else BiNode (i a l) v r
        i a EmptyTree = leaf a
        compare' = if b
                      then (<)
                      else (>=)


bottom' :: Ord a => (a -> b) -> (b -> b) -> Heap a -> ZTree b
bottom' m f (Heap EmptyTree) = (fmap m . zTop) EmptyTree
bottom' m f (Heap t) = recr (fmap m (zTop t))
  where recr (ZTree t c) = 
          case t of
            BiNode l _ r -> 
              if shallow l > shallow r
                 then (recr . ztRight . ztModify f) (ZTree t c)
                 else (recr . ztLeft . ztModify f) (ZTree t c)
            _ -> ztModify f (ZTree t c)

bottomBy :: (a -> Bool) -> BiTree a -> ZTree a
bottomBy base EmptyTree = zTop EmptyTree
bottomBy base t = recr (zTop t)
  where recr (ZTree t c) = 
          case t of
            BiNode l v r -> 
              if base v
                 then ZTree t c
                 else if shallowBy base l > shallowBy base r
                         then (recr . ztRight) (ZTree t c)
                         else (recr . ztLeft) (ZTree t c) 
            _ -> ztUp (ZTree t c)

bottom :: Ord a => Heap a -> ZTree a
bottom h = bottom' id id h

shallowBy :: (a -> Bool) -> BiTree a -> Int
shallowBy _ EmptyTree = 0
shallowBy base (BiNode l v r) = 
  if base v
     then 0
     else (min (shallowBy base l) (shallowBy base r)) + 1
     
depthBy :: (a -> Bool) -> BiTree a -> Int
depthBy _ EmptyTree = 0
depthBy base (BiNode l v r) = 
  if base v
     then 0
     else (max (depthBy base l) (depthBy base r)) + 1

lastElemBy :: (a -> Bool) -> BiTree a -> ZTree a
lastElemBy base t = recr (zTop t)
  where recr (ZTree t c) = 
          case t of
            BiNode l v r -> 
              if base v
                 then ztUp (ZTree t c)
                 else if depthBy base r >= depthBy base l
                         then (recr . ztRight) (ZTree t c)
                         else (recr . ztLeft) (ZTree t c) 
            _ -> ztUp (ZTree t c)

lastElem :: Ord a => Heap a -> ZTree a
lastElem (Heap t) = recr (zTop t)
  where recr (ZTree t c) = 
          case t of
            BiNode l _ r -> 
              if depth r >= depth l
                 then (recr . ztRight) (ZTree t c)
                 else (recr . ztLeft) (ZTree t c)
            _ -> ztUp (ZTree t c)

upHeap :: (a -> a) -> EditTree a -> EditTree a
upHeap mark (EditTree (ZTree (BiNode l v r) c)) = 
  case c of
    L u k p -> (EditTree . ztUp) 
                 (ZTree (BiNode l (mark u) r) (L v k p))
    R s u k -> (EditTree . ztUp) 
                 (ZTree (BiNode l (mark u) r) (R s v k))
    Top -> EditTree (ZTree (BiNode l v r) Top)
upHeap _ et = et -- Nothing happens if you try to upHeap an EmptyTree

upHeapZ :: (a -> a) -> ZTree a -> ZTree a
upHeapZ mark (ZTree (BiNode l v r) c) = 
  case c of
    L u k p -> (ztUp) (ZTree (BiNode l (mark u) r) (L v k p))
    R s u k -> (ztUp) (ZTree (BiNode l (mark u) r) (R s v k))
    Top -> ZTree (BiNode l v r) Top
upHeapZ _ et = et -- Nothing happens if you try to upHeap an EmptyTree

downHeapL :: (a -> a) -> EditTree a -> EditTree a
downHeapL mark (EditTree (ZTree (BiNode (BiNode l u p) v r) c)) = 
  (EditTree . ztLeft) (ZTree (BiNode (BiNode l v p) (mark u) r) c)
downHeapL _ et = et

downHeapLZ :: (a -> a) -> ZTree a -> ZTree a
downHeapLZ mark (ZTree (BiNode (BiNode l u p) v r) c) = 
  ztLeft (ZTree (BiNode (BiNode l v p) (mark u) r) c)
downHeapLZ _ et = et

downHeapR :: (a -> a) -> EditTree a -> EditTree a
downHeapR mark (EditTree (ZTree (BiNode l v (BiNode s u r)) c)) = 
  (EditTree . ztRight) (ZTree (BiNode l (mark u) (BiNode s v r)) c)
downHeapR _ et = et

downHeapRZ :: (a -> a) -> ZTree a -> ZTree a
downHeapRZ mark (ZTree (BiNode l v (BiNode s u r)) c) = 
  ztRight (ZTree (BiNode l (mark u) (BiNode s v r)) c)
downHeapRZ _ et = et

type NodeForm a = (ZTree a -> (SuperForm, LineForm))

type LineForm = (Location -> SuperForm)

type Embedding a = (ZTree a -> Location)

toForm :: BoundingBox -> Embedding a -> NodeForm a -> BiTree a -> SuperForm
toForm bb a b c = nextNode bb a b (zTop c)

nextNode :: BoundingBox -> Embedding a -> NodeForm a -> ZTree a -> SuperForm
nextNode bb findLoc nodeForm zt = 
  case zt of
    ZTree (BiNode _ _ _) _ -> 
      let loc = bb * findLoc zt
          lineDest = bb * findLoc (ztUp zt) - loc
          (node,lineF) = nodeForm zt
          line = lineF lineDest

          next = nextNode bb findLoc nodeForm

      in combine [translate loc line
                 ,next (ztLeft zt)
                 ,next (ztRight zt)
                 ,fit loc ((0.7,0.7) * bb) node]
    _ -> blank

class DrawableNode n where
  nodeForm :: n -> (SuperForm, LineForm)

instance DrawableNode a => DrawableNode (ZTree a) where
  nodeForm (ZTree (BiNode _ v _) _) = nodeForm v
  nodeForm _ = (blank, const blank)

zDepthOf :: ZTree a -> Int
zDepthOf (ZTree _ Top) = 1
zDepthOf t = ((+1) . zDepthOf . ztUp) t

zFindLoc :: ZTree a -> Location
zFindLoc tree = (fromIntegral (findX tree)
                ,fromIntegral (zDepthOf tree - 1))

findX :: ZTree a -> Int
findX zt = case zt of
             ZTree (BiNode l _ r) Top -> 
               let lsubtree = (zTree . ztLeft) zt
                   initXIndex = 0
               in nextX initXIndex lsubtree
             ZTree (BiNode l _ r) (L _ _ _) -> 
               let rsubtree = (zTree . ztRight) zt
                   initXIndex = 0
               in findX (ztUp zt) - nextX initXIndex rsubtree - 1
             ZTree (BiNode l _ r) (R _ _ _) -> 
               let lsubtree = (zTree . ztLeft) zt
                   initXIndex = 0
               in findX (ztUp zt) + nextX initXIndex lsubtree + 1
  where nextX :: Int -> BiTree a -> Int
        nextX x (BiNode EmptyTree _ EmptyTree) =
          x + 1 -- leaf node case
        nextX x EmptyTree =
          x -- absent child case
        nextX x (BiNode l _ r) =
          let x' = nextX x l 
          in nextX (x' + 1) r
