{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe
import Data.Monoid
import Text.Read (readMaybe)
import System.Random
import qualified Data.Map as M
import qualified Data.List as L

import Super.Canvas
import Super.Trees
import Super.Trees2

data Config = Config { useAnimations :: Bool
                     , ordDirection :: Bool
                     , defaultTreeSize :: Int
                     , treeSizeInputID :: String
                     , newGameButtonID :: String
                     , freezeFrameDelay :: Int
                     , animationFrameDelay :: Int }

main = do let n = "main"
              s = "background: lightgray;"
          conf <- Config
                  <$> option n "use-animations" True
                  <*> option n "ord-direction" True
                  <*> option n "default-tree-size" 8
                  <*> option n "tree-size-input-id" "numnodes"
                  <*> option n "new-game-button-id" "restart"
                  <*> option n "freeze-frame-delay" 1000
                  <*> option n "animation-frame-delay" 100
          sc <- startCanvas n
                            (900,500) 
                            s
                            ["main","message","frame","defbuttons"]
                            (const (return ()))
          startHeapGame (sc,conf)

type StateModifier = GameState -> Writer [IO ()] GameState

data Env = Env { sc :: SuperCanvas
               , conf :: Config
               , runM :: StateModifier -> IO () }

nodesize = (30,40)


startHeapGame :: (SuperCanvas,Config) -> IO ()
startHeapGame (sc,conf) = 
  do g <- newStdGen
     (gameManips,runManip) <- newAddHandler
     let env = Env sc conf runManip
     attachButton (newGameButtonID conf) 
                  (restartGame env <$> readNewGame env) runManip
     initialGame <- readNewGame env
     compile (heapGame env
                       (initialGame, [writeState env initialGame])
                       gameManips
                       runManip) >>= actuate
     writeState env initialGame
     writeS sc "frame" frameForm

frameForm = rekt (15,15) (870,470) False Black

restartGame :: Env -> GameState -> StateModifier
restartGame env newGame _ = tell [writeState env newGame] 
                            >> return newGame

readNewGame :: Env -> IO GameState
readNewGame env = 
  do num <- safeReadInput (treeSizeInputID (conf env)) 
                          (defaultTreeSize (conf env))
     g <- newStdGen
     let nodes = take num (fmap HeapNode (randomRs randRange g))
         heap = foldr (insert (ordDirection (conf env))) newHeap nodes
     changeInput (treeSizeInputID (conf env)) (show num)
     return (Valid heap)

heapGame env iGame gameMs runM = 
  do eGameMs <- fromAddHandler gameMs
     let gstate = fst <$> bGameM
         vstate = snd <$> bGameM
         bGameM =  accumB iGame (update <$> eGameMs)
     visuals <- changes vstate
     reactimate' (fmap sequence_ <$> visuals)
     return ()
     
update :: StateModifier -> (GameState,[IO ()]) -> (GameState,[IO ()])
update m (gs,_) = runWriter (m gs)

visualize :: DrawableNode (QNode a b) 
          => Env -> Int -> Int -> String -> VTrace a b -> IO ()
visualize _ _ _ _ [] = return ()
visualize env d finald str vt = 
  let w i = animateS (sc env) "main" 1 i
            . fitTreeArea env
            . toForm nodesize zFindLoc nodeForm
            . zTree
            . ztUpMost
      -- str = "Validating..."
  in dumbButtons env
     >> if (useAnimations . conf) env
           then (sequence_ . fmap (w d)) vt
                >> messageHold env White (d * length vt) str
                >> w finald (L.last vt)
           else w finald (L.last vt)

--   >> (sequence_ . fmap (animateS (sc env) "main" 1 d
--                         . fitTreeArea env
--                         . toForm nodesize zFindLoc nodeForm 
--                         . zTree
--                         . ztUpMost)) vt
--   >> animateS (sc env) "main" 1 finald 

writeState :: Env -> GameState -> IO ()
writeState env (Valid (Heap t)) = 
  do g <- newStdGen
     let tree = fitTreeArea env (toForm nodesize 
                                        zFindLoc 
                                        normalNodeForm 
                                        (fmap (makeQ Unfocused) t))
         doRemMin = (runM env) (modRemoveMin env)
         bRemMin = (fitControl2 env . addOnClick [doRemMin]) 
                     (buttonForm "Remove Min" LightPurple)
         newNode = (HeapNode . fst) (randomR randRange g)
         doAddNew = (runM env) (modInsertNew env newNode)
         bAddNew = (fitControl1 env . addOnClick [doAddNew]) 
                     (buttonForm "Insert New" LightBlue)
     (write (sc env) "main" . combine) [tree,bRemMin,bAddNew]
     message env LightGreen "The heap is valid."
writeState env (RemoveMin (EditTree t)) = 
  writeEditState env t (rmNodeForm env)
writeState env (InsertNew (EditTree t)) = 
  writeEditState env t (insNodeForm env)
writeState env (GameOver) = 
  message env LightRed "Invalid heap! Violations are marked in red."
  >> dumbButtons env

dumbButtons :: Env -> IO ()
dumbButtons env = 
  writeS (sc env) 
         "defbuttons" 
         (combine [fitControl1 env (buttonForm "" Gray)
                  ,fitControl2 env (buttonForm "" Gray)])

clearDumbButtons env = writeS (sc env) "defbuttons" blank

messageForm :: Color -> String -> SuperForm
messageForm col str = combine [rekt (0,0) (500,30) True col
                              ,rekt (0,0) (500,30) False Black
                              ,text (250,15) (490,15) str]

buttonForm :: String -> Color -> SuperForm
buttonForm str col = combine [rekt (1,1) (160,30) False Black
                             ,rekt (0,0) (160,30) True col
                             ,rekt (0,0) (160,30) False Black
                             ,text (80,15) (155,17) str]

writeEditState env t nf = 
  let tree = fitTreeArea env (toForm nodesize 
                                     zFindLoc 
                                     nf 
                                     (zTree (ztUpMost t)))
      doCommit = (runM env) (modValidate env)
      bCommit = (fitControl1 env . addOnClick [doCommit]) 
                  (buttonForm "Validate" Orange)
      blank2 = fitControl2 env (buttonForm "" Gray)
  in (write (sc env) "main" . combine) [tree,bCommit,blank2]
     >> message env LightYellow "Edit Mode: Correct the heap if necessary."

message :: Env -> Color -> String -> IO ()
message env col = writeS (sc env) "message" 
                  . fitMessageArea env 
                  . messageForm col

messageHold :: Env -> Color -> Int -> String -> IO ()
messageHold env col del str = 
  animateS (sc env) "message" 1 del (fitMessageArea 
                                       env (messageForm 
                                              col str))

normalNodeForm (ZTree (BiNode _ n _) _) = nodeForm n
normalNodeForm _ = (blank, const blank)

rmNodeForm :: Env -> NodeForm (QNode HeapNode Focus)
rmNodeForm env zt = 
  let (form,line) = nodeForm zt
  in case zt of
       ZTree _ (L (QNode _ Focused) _ _) -> 
         (combine [addOnClick [(runM env) (modSwap env downHeapL)] form
                  ,highlight env]
         ,line)
       ZTree _ (R _ (QNode _ Focused) _) -> 
         (combine [addOnClick [(runM env) (modSwap env downHeapR)] form
                  ,highlight env]
         ,line)
       _ -> (form,line)

insNodeForm :: Env -> NodeForm (QNode HeapNode Focus)
insNodeForm env zt = 
  let (form,line) = nodeForm zt
  in case zt of
       ZTree (BiNode (BiNode _ (QNode _ Focused) _) _ _) _ -> 
         (combine [addOnClick [(runM env) (modSwap env upHeap)] form
                  ,highlight env]
         ,line)
       ZTree (BiNode _ _ (BiNode _ (QNode _ Focused) _)) _ -> 
         (combine [addOnClick [(runM env) (modSwap env upHeap)] form
                  ,highlight env]
         ,line)
       _ -> (form,line)

modSwap :: Env 
        -> (EditTree (QNode HeapNode Focus) -> EditTree (QNode HeapNode Focus)) 
        -> StateModifier
modSwap env mod (RemoveMin et) = 
  let state = RemoveMin (mod et)
  in tell [writeState env state] >> return state
modSwap env mod (InsertNew et) = 
  let state = InsertNew (mod et)
  in tell [writeState env state] >> return state
modSwap _ _ s = return s

modValidate :: Env -> StateModifier
modValidate env (RemoveMin et) = modValidateValid env validateRemoveM et
modValidate env (InsertNew et) = modValidateValid env validateInsertM et
modValidate _ s = return s

modValidateValid :: Env
                 -> ValidateM HeapNode
                 -> EditTree (QNode HeapNode Focus) 
                 -> Writer [IO ()] GameState
modValidateValid env validator et = 
  let (tree,trace) = validator env et
      state = case tree of
                Just h -> Valid h
                _ -> GameOver
      frDel = freezeFrameDelay (conf env)
      anDel = animationFrameDelay (conf env)
  in tell [(visualize env anDel frDel "Validating heap..." trace)] 
     >> tell [(writeState env state)]
     >> return state

modRemoveMin :: Env -> StateModifier
modRemoveMin env (Valid h) = 
  tell [ visualize env 0 frDel "Removing element at head of heap..." pre
       , writeState env state] >> return state
  where state = RemoveMin (removeMin h)
        pre = [preRemove h]
        frDel = freezeFrameDelay (conf env)
modRemoveMin _ s = return s

modInsertNew :: Env -> HeapNode -> StateModifier
modInsertNew env n (Valid h) = 
  tell [writeState env state] >> return state
  where state = InsertNew (carelessInsert n h)
modInsertNew _ _ s = return s

fitTreeArea :: Env -> SuperForm -> SuperForm
fitTreeArea env sf = let treeAreaX = 810
                         ff = fit (45,120) (810,350) sf
                         xv = (fst (snd (bounds ff))) / 2
                     in translate (treeAreaX / 2 - xv + 8,0) ff

fitControl1 :: Env -> SuperForm -> SuperForm
fitControl1 env = fit (45,35) (150,100)

fitControl2 :: Env -> SuperForm -> SuperForm
fitControl2 env = fit (210,35) (150,100)

fitMessageArea :: Env -> SuperForm -> SuperForm
fitMessageArea env = fit (375,35) (480,100)

type HeapTree = BiTree (Int, Bool)

data HeapNode = HeapNode Int deriving (Show, Eq, Ord)

instance DrawableNode HeapNode where
  nodeForm (HeapNode v) = (text (0,0) (200,100) (show v)
                          ,(\ploc -> line (0,0) ploc 2 Black))

data QNode a s = QNode { qVal :: a
                       , qStatus :: s } deriving (Show)

instance DrawableNode (QNode HeapNode Focus) where
  nodeForm (QNode h Focused) = qForm h Orange
  nodeForm (QNode h OnPath) = qForm h LightYellow
  nodeForm (QNode h Unfocused) = qForm h White

instance DrawableNode (QNode HeapNode Status) where
  nodeForm (QNode h Unchecked) = nodeForm (QNode h Unfocused)
  nodeForm (QNode h OfInterest) = nodeForm (QNode h OnPath)
  nodeForm (QNode h Marker) = nodeForm (QNode h Focused)
  nodeForm (QNode h Good) = 
    let (f,_) = qForm h LightGreen
        l = (\ploc -> line (0,0) ploc 4 Green)
    in (f,l)
  nodeForm (QNode h BadChild) = 
    let (f,_) = qForm h LightRed
        l = (\ploc -> line (0,0) ploc 4 Red)
    in (f,l)
  nodeForm (QNode h BadParent) = qForm h LightRed

qForm :: HeapNode -> Color -> (SuperForm, LineForm) 
qForm h c = let (t,line) = nodeForm h
                r = rekt (-150,-100) (300,200) True c
                o = rekt (-150,-100) (300,200) False Black
            in (combine [r,o,t], line) 

highlight :: Env -> SuperForm
highlight env = combine [circle (-143,-100) 48 True Orange
                        -- ,circle (-143,-100) 41 False Orange
                        -- ,circle (-143,-100) 40 False Orange
                        -- ,circle (-143,-100) 39 False Orange
                        ]

instance Eq a => Eq (QNode a s) where
  (==) (QNode a _) (QNode b _) = a == b

instance (Eq a, Ord a) => Ord (QNode a s) where
  compare (QNode a _) (QNode b _) = compare a b

data Focus = Focused | Unfocused | OnPath deriving (Show)

data Status = Unchecked
            | OfInterest
            | Marker
            | Good 
            | BadChild 
            | BadParent deriving (Show)

setQ :: q -> QNode a s -> QNode a q
setQ q (QNode n _) = QNode n q

makeQ :: q -> a -> QNode a q
makeQ q a = QNode a q 

carelessInsert :: Ord a => a -> Heap a -> EditTree (QNode a Focus)
carelessInsert a h = (EditTree 
                      . ztReplace (makeQ Focused a)
                      . bottom' (makeQ Unfocused) (setQ OnPath)) h

removeMin :: Ord a => Heap a -> EditTree (QNode a Focus)
removeMin (Heap EmptyTree) = EditTree (zTop EmptyTree)
removeMin h = (EditTree 
               . ztReplace (setQ Focused v) 
               . ztUpMost 
               . ztCut) (ZTree b c)
  where (ZTree b c) = (fmap (makeQ Unfocused) . lastElem) h
        (BiNode _ v _) = b

preRemove :: Ord a => Heap a -> ZTree (QNode a Focus)
-- preRemove (Heap EmptyTree) = ZTree (zTop EmptyTree)
preRemove = (ztModify (setQ Focused) . fmap (makeQ Unfocused) . lastElem)

data HeapGame = HeapGame { hgScore :: Int
                         , hgState :: GameState } deriving (Show)

data GameState = Valid (Heap HeapNode)
               | RemoveMin (EditTree (QNode HeapNode Focus))
               | InsertNew (EditTree (QNode HeapNode Focus))
               | GameOver
               deriving (Show)

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

stamp :: s -> ZTree (QNode a s) -> ZTree (QNode a s)
stamp s (ZTree (BiNode l (QNode v _) r) c) = 
  ZTree (BiNode l (QNode v s) r) c
stamp _ t = t

type Validator a = MaybeT (Writer (VTrace a Status)) 
                          (ZTree (QNode a Status))

type VTrace a b = [ZTree (QNode a b)]

-- validateM :: Ord a 
--           => (EditTree (QNode a s)) 
--           -> (Maybe (Heap a), VTrace a Status)
validateM :: Ord a => ValidateM a
validateM env (EditTree t) = 
  case nt of
    (ZTree (BiNode _ _ _) _) -> 
      let (res,trace) = runWriter (runMaybeT (markValid nt >>= check))
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

type ValidateM a = Env -> (EditTree (QNode a Focus)) -> (Maybe (Heap a), VTrace a Status)

-- validateInsertM :: Ord a 
--                 => (EditTree (QNode a Focus))
--                 -> (Maybe (Heap a), VTrace a Status)
validateInsertM :: Ord a => ValidateM a
validateInsertM env (EditTree t) = 
  case nt of
    ZTree (BiNode _ _ _) _ -> 
      let (res,trace) = runWriter (runMaybeT (checkUp env nt))
      in (fmap (Heap . zTree . ztUpMost . fmap qVal) res, trace)
    _ -> (Just (Heap EmptyTree), [])
  where nt = (fmap path2Interest . lastElem . Heap . zTree . ztUpMost) t

path2Interest :: QNode a Focus -> QNode a Status
path2Interest (QNode n Focused) = QNode n Marker
path2Interest (QNode n OnPath) = QNode n OfInterest
path2Interest (QNode n _) = QNode n Unchecked

checkUp :: Ord a => Env -> ZTree (QNode a Status) -> Validator a
checkUp env zt = 
  case zt of 
    ZTree (BiNode _ (QNode v OfInterest) _) (L u _ _) -> 
      evalNodes (QNode v OfInterest) u >>= (checkUp env . ztUp)
    ZTree (BiNode _ (QNode v OfInterest) _) (R _ u _) -> 
      evalNodes (QNode v OfInterest) u >>= (checkUp env . ztUp)
    ZTree (BiNode _ (QNode v Marker) _) (L u _ _) -> 
      evalNodes (QNode v Marker) u
    ZTree (BiNode _ (QNode v Marker) _) (R _ u _) -> 
      evalNodes (QNode v Marker) u
    _ -> return zt
  where evalNodes v u = if compare' u v
                           then markValid' zt
                           else markFail' zt
        markValid' zt = let nxt = stamp Good zt
                        in tell [nxt] >> return nxt
        markFail' zt = let nxt = (stamp BadParent
                                  . ztUp
                                  . stamp BadChild) zt
                       in tell [nxt] >> fail "Invalid Heap"
        compare' = if ordDirection (conf env)
                      then (<=)
                      else (>)

validateRemoveM :: Ord a => ValidateM a
validateRemoveM env (EditTree t) = 
  case nt of
    ZTree (BiNode _ _ _) _ -> 
      let (res,trace) = runWriter (runMaybeT (checkAround env nt))
      in (fmap (Heap . zTree . ztUpMost . fmap qVal) res, trace)
    _ -> (Just (Heap EmptyTree), [])
  where nt = fmap (setQ Unchecked) t
  
checkAround :: Ord a => Env -> ZTree (QNode a Status) -> Validator a
checkAround env zt = case zt of
                       ZTree (BiNode _ v _) Top -> 
                         do evalTree v (ztLeft zt)
                            zt2 <- ztUp <$> markValid' (ztLeft zt)
                            evalTree v (ztRight zt2) 
                            zt3 <- ztUp <$> markValid' (ztRight zt2)
                            markValid' zt3
                       ZTree (BiNode _ v _) _ -> 
                         do evalTree v (ztLeft zt)
                            zt2 <- ztUp <$> markValid' (ztLeft zt)
                            evalTree v (ztRight zt2)
                            zt3 <- ztUp <$> markValid' (ztRight zt2)
                            markValid' zt3 >>= (checkAround env . ztUp)
  where evalTree u (ZTree EmptyTree _) = return ()
        evalTree u (ZTree (BiNode l v r) c) 
          = if compare' u v
               then return ()
               else markFail' (ZTree (BiNode l v r) c)
        markValid' zt = let nxt = stamp Good zt
                        in tell [nxt] >> return nxt
        markFail' zt = let nxt = (stamp BadParent
                                  . ztUp
                                  . stamp BadChild) zt
                       in tell [nxt] >> fail "Invalid Heap"
        compare' = if ordDirection (conf env)
                      then (<=)
                      else (>)
