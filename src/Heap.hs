{-# LANGUAGE FlexibleInstances #-}

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe
import Data.Monoid
import Text.Read (readMaybe)
import System.Random
import qualified Data.Map as M

import Super.Canvas
import Super.Trees
import Super.Trees2

main = startCanvas "main" 
                   (900,500) 
                   "background: lightgray;"
                   ["main"]
                   (\_ -> return ())
       >>= treestuff

type HeapTree = BiTree (Int, Bool)

data HeapNode = HeapNode Int deriving (Eq, Ord)

instance DrawableNode HeapNode where
  nodeForm (HeapNode v) = (text (0,0) (200,100) (show v)
                          ,(\ploc -> line (0,0) ploc 2 Black))

data QNode a s = QNode { qVal :: a
                       , qStatus :: s }
                       
instance DrawableNode (QNode HeapNode Focus) where
  nodeForm (QNode h Focused) = qForm h Yellow
  nodeForm (QNode h Unfocused) = qForm h White

instance DrawableNode (QNode HeapNode Status) where
  nodeForm (QNode h Unchecked) = nodeForm (QNode h Unfocused)
  nodeForm (QNode h Good) = 
    let (f,_) = qForm h Green
        l = (\ploc -> line (0,0) ploc 4 Green)
    in (f,l)
  nodeForm (QNode h BadChild) = 
    let (f,_) = qForm h Red
        l = (\ploc -> line (0,0) ploc 4 Red)
    in (f,l)
  nodeForm (QNode h BadParent) = qForm h Red

qForm :: HeapNode -> Color -> (SuperForm, LineForm) 
qForm h c = let (t,line) = nodeForm h
                r = rekt (-200,-100) (400,200) True c
            in (combine [r,t], line) 

instance Eq a => Eq (QNode a s) where
  (==) (QNode a _) (QNode b _) = a == b

instance (Eq a, Ord a) => Ord (QNode a s) where
  compare (QNode a _) (QNode b _) = compare a b

data Focus = Focused | Unfocused

data Status = Unchecked | Good | BadChild | BadParent

setQ :: q -> QNode a s -> QNode a q
setQ q (QNode n _) = QNode n q

makeQ :: q -> a -> QNode a q
makeQ q a = QNode a q 

carelessInsert :: Ord a => a -> Heap a -> EditTree (QNode a Focus)
carelessInsert a h = (EditTree 
                      . ztReplace (makeQ Focused a)
                      . fmap (makeQ Unfocused)
                      . bottom) h

removeMin :: Ord a => Heap a -> EditTree (QNode a Focus)
removeMin (Heap EmptyTree) = EditTree (zTop EmptyTree)
removeMin h = (EditTree 
               . ztReplace (setQ Focused v) 
               . ztUpMost 
               . ztCut) (ZTree b c)
  where (ZTree b c) = (fmap (makeQ Unfocused) . bottom) h
        (BiNode _ v _) = b

data HeapGame = HeapGame { hgScore :: Int
                         , hgState :: GameState }
                         
data GameState = Valid (Heap HeapNode)
               | Edit (EditTree HeapNode)
               | GameOver SuperForm

newGame :: [HeapNode] -> HeapGame
newGame ns = HeapGame 0 (Valid (Heap (makeHeap ns))) 

treestuff sc = 
  do t <- newAddHandler
     g <- newStdGen
     b <- newAddHandler
     attachButton "newnode" newStdGen (snd b)
     let thetrees :: HeapTree
         thetrees = randomHeapTree 8 g
     network <- compile (mkNet sc
                               (fst t)
                               (fst b)
                               (randomRs randRange g)
                               (snd t))
     actuate network
    -- write sc (format (prepSTree (fst thetrees)))
     (snd t) thetrees
     putStrLn "Started?"
     return ()

mkNet sc t b rs fire =
  do eTrees <- fromAddHandler t
     eButton <- fromAddHandler b
     let bAdd = stepper (\a -> EmptyTree) 
                        (fmap addNode' eAllTrees)
         eAllTrees = eTrees `union` (bAdd <@> eButton)
         eTreeForms = fmap (format4 fire) eAllTrees
         eForms = eTreeForms
     reactimate (fmap (write sc "main") eForms)


addNode' t g = addNode g t

addNode :: StdGen -> HeapTree -> HeapTree
addNode g = (\(a,_) -> a)
            . qtUpMost
            . insertNew' (newNode g) g
            . (\a -> (a,Top))
            . clean

randRange = (10,99)

newNode g = (fst $ randomR randRange g, True)

clean :: HeapTree -> HeapTree
clean = fmap (\(v,_) -> (v,False))

randomHeapTree i g = 
  makeHeap (fmap (\a -> (a,False)) (take i (randomRs randRange g)))

insertNew' node _ (t,c) = (heapCarelessInsert node t, c)

insertNew node g qt = 
  ( (\(t,c) -> (BiNode EmptyTree node EmptyTree,c))
  . randomChild g ) qt

format :: SuperForm -> SuperForm
format = translate (50,50)

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n



format4 fire trees = fit (50,50) (800,400) (prepHeapTree fire trees)

validate :: Ord a => (a -> a -> Bool) -> EditTree a -> Maybe (Heap a)
validate comp (EditTree t) = 
  case (ztUpMost t) of
    (ZTree (BiNode l v r) _) -> 
      if valid v l && valid v r 
         then Just (Heap (BiNode l v r))
         else Nothing
    _ -> Just (Heap EmptyTree)
  where valid v (BiNode l u r) = 
          (comp v u) && valid v l && valid v r
        valid _ _ = True

-- validateM :: (Ord a, Monoid w) 
--           => (a -> a -> Bool) 
--           -> EditTree a 
--           -> Writer w (Maybe (Heap a))
-- validateM comp (EditTree t) = 
--   case nt of
--     (ZTree (BiNode l v r) _) -> 
--       do c <- check nt
--          if c
--             then return ((Just . Heap . zTree . fmap qVal) t)
--             else return Nothing
--   where nt = (fmap (setQ Unchecked) . ztUpMost) t
--             
-- check :: (Ord a, Monoid w) 
--       => (a -> a -> Bool) 
--       -> ZTree (QNode a Status) 
--       -> Writer w Bool
-- check comp nt = 
--   case nt of
--     (ZTree (BiNode _ v _) _) -> 
--       do lv <- follow comp v (qtLeft nt)
--          rv <- follow comp v (qtRight nt)
--          if lv
--             then if rv
--                     then tell nt >> return True
--                     else undefined
--             else undefined

stamp :: s -> ZTree (QNode a s) -> ZTree (QNode a s)
stamp s (ZTree (BiNode l (QNode v _) r) c) = 
  ZTree (BiNode l (QNode v s) r) c
stamp _ t = t

type Validator a = MaybeT (Writer (VTrace a)) 
                          (ZTree (QNode a Status))

type VTrace a = [ZTree (QNode a Status)]

validateM :: Ord a 
          => (EditTree (QNode a s)) 
          -> (Maybe (Heap a), VTrace a)
validateM (EditTree t) = 
  case nt of
    (ZTree (BiNode _ _ _) _) -> 
      let (res,trace) = runWriter (runMaybeT (check nt))
      in (fmap (Heap . zTree . ztUpMost . fmap qVal) res 
         ,trace)
    _ -> (Just (Heap EmptyTree), [])
  where nt = (fmap (setQ Unchecked) . ztUpMost) t
  
check :: Ord a => ZTree (QNode a Status) -> Validator a
check zt = ztUp <$> case zt of
                      (ZTree (BiNode l v r) c) -> investigate zt
                      _ -> return zt

investigate zt = (checkThisNode zt 
                  >>= (check . ztLeft) 
                  >>= (check . ztRight))

checkThisNode :: Ord a => ZTree (QNode a Status) -> Validator a
checkThisNode zt = (lookAt v . ztLeft) zt >>= (lookAt v . ztRight)
  where (ZTree (BiNode _ v _) _) = zt

lookAt :: Ord a => QNode a Status -> ZTree (QNode a Status) -> Validator a
lookAt u zt = case zt of
                (ZTree (BiNode _ v _) c) -> 
                  if u <= v
                     then markValid zt
                     else markFail zt
                _ -> return (ztUp zt)

markValid :: Ord a => ZTree (QNode a Status) -> Validator a
markValid zt = let nxt = (ztUp . stamp Good) zt
               in tell [nxt] >> return nxt

markFail :: Ord a => ZTree (QNode a Status) -> Validator a
markFail zt = let nxt = (stamp BadParent 
                         . ztUp 
                         . stamp BadChild) zt
              in tell [nxt] >> fail "Invalid Heap"
