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
                     then BiNode (i a l) v r
                     else BiNode l v (i a r)
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

toForm :: Embedding a -> NodeForm a -> BiTree a -> SuperForm
toForm a b c = nextNode a b (zTop c)

nextNode :: Embedding a -> NodeForm a -> ZTree a -> SuperForm
nextNode findLoc nodeForm zt = 
  let loc = findLoc zt
      lineDest = findLoc (ztUp zt)
      
      (node,lineF) = nodeForm zt
      line = lineF lineDest
      
      next = nextNode findLoc nodeForm
      
  in combine [translate loc line
             ,next (ztLeft zt)
             ,next (ztRight zt)
             ,translate loc node]
             
class DrawableNode n where
  nodeForm :: n -> (SuperForm, LineForm)
