module Super.Trees2 where

import Super.Canvas
import Super.Trees

data EditTree a = EditTree { etTree :: (ZTree a) }

data Ord a => Heap a = Heap { hTree :: (BiTree a) }

newHeap :: Ord a => Heap a
newHeap = Heap EmptyTree

insert :: Ord a => a -> Heap a -> Heap a
insert a (Heap t) = Heap (i a t)
  where i a (BiNode l v r) = 
          if a < v
             then i v (BiNode l a r)
             else if depth l > depth r
                     then BiNode l v (i a r)
                     else BiNode (i a l) v r
        i a EmptyTree = leaf a


bottom :: Ord a => Heap a -> ZTree a
bottom (Heap EmptyTree) = zTop EmptyTree
bottom (Heap t) = recr (zTop t)
  where recr (ZTree t c) = 
          case t of
            BiNode l _ r -> 
              if depth l > depth r
                 then (recr . ztRight) (ZTree t c)
                 else (recr . ztLeft) (ZTree t c)
            _ -> ZTree t c



upHeap :: EditTree a -> EditTree a
upHeap (EditTree (ZTree (BiNode l v r) c)) = 
  case c of
    L u k p -> EditTree (ZTree (BiNode l u r) (L v k p))
    R s u k -> EditTree (ZTree (BiNode l u r) (R s v k))
    Top -> EditTree (ZTree (BiNode l v r) Top)
upHeap et = et -- Nothing happens if you try to upHeap an EmptyTree

downHeapL :: EditTree a -> EditTree a
downHeapL (EditTree (ZTree (BiNode (BiNode l u p) v r) c)) = 
  EditTree (ZTree (BiNode (BiNode l v p) u r) c)
downHeapL et = et

downHeapR :: EditTree a -> EditTree a
downHeapR (EditTree (ZTree (BiNode l v (BiNode s u r)) c)) = 
  EditTree (ZTree (BiNode l u (BiNode s v r)) c)
downHeapR et = et

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
                 ,translate loc node]
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
