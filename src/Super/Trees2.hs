module Super.Trees2 where

import Super.Canvas
import Super.Trees

leaf :: a -> BiTree a
leaf a = BiNode EmptyTree a EmptyTree

data EditTree a = EditTree (QualTree a)

class Edit e where
  edit :: Ord a => e a -> EditTree a

data Ord a => Heap a = Heap (BiTree a)

instance Edit Heap where 
  edit (Heap tree) = EditTree (top tree)
  
insert :: Ord a => a -> Heap a -> Heap a
insert a (Heap t) = Heap (i a t)
  where i a (BiNode l v r) = 
          if a > v
             then i v (BiNode l a r)
             else if depth l > depth r
                     then BiNode (i a l) v r
                     else BiNode l v (i a r)
        i a EmptyTree = leaf a

qtReplace :: a -> QualTree a -> QualTree a
qtReplace v (BiNode l _ r,c) = (BiNode l v r,c)
qtReplace _ qt = qt

qtCut :: QualTree a -> QualTree a
qtCut (_,L v c r) = (BiNode EmptyTree v r,c)
qtCut (_,R l v c) = (BiNode l v EmptyTree,c)
qtCut (_,Top) = (EmptyTree,Top)

bottom :: Ord a => Heap a -> QualTree a
bottom (Heap EmptyTree) = top EmptyTree
bottom (Heap t) = recr (top t)
  where recr (t,c) = 
          case t of
            BiNode l _ r -> if depth l > depth r
                               then (recr . qtRight) (t,c)
                               else (recr . qtLeft) (t,c)
            _ -> (t,c)

carelessInsert :: Ord a => a -> Heap a -> EditTree a
carelessInsert a = EditTree . qtReplace a . bottom

removeMin :: Ord a => Heap a -> EditTree a
removeMin (Heap EmptyTree) = EditTree (top EmptyTree)
removeMin h = (EditTree . qtReplace v . qtUpMost . qtCut) (b,c)
  where (b,c) = bottom h
        (BiNode _ v _) = b

upHeap :: EditTree a -> EditTree a
upHeap (EditTree (BiNode l v r,c)) = 
  case c of
    L u k p -> EditTree (BiNode l u r, L v k p)
    R s u k -> EditTree (BiNode l u r, R s v k)
    Top -> EditTree (BiNode l v r, Top)
upHeap et = et -- Nothing happens if you try to upHeap an EmptyTree

downHeapL :: EditTree a -> EditTree a
downHeapL (EditTree (BiNode (BiNode l u p) v r,c)) = 
  EditTree (BiNode (BiNode l v p) u r,c)
downHeapL et = et

downHeapR :: EditTree a -> EditTree a
downHeapR (EditTree (BiNode l v (BiNode s u r),c)) = 
  EditTree (BiNode l u (BiNode s v r),c)
downHeapR et = et

validate :: Ord a => (a -> a -> Bool) -> EditTree a -> Maybe (Heap a)
validate comp (EditTree (BiNode l v r,_)) = 
  if valid v l && valid v r 
     then Just (Heap (BiNode l v r))
     else Nothing
  where valid v (BiNode l u r) = 
          (comp v u) && valid v l && valid v r
        valid _ _ = True
validate _ _ = Just (Heap EmptyTree)
