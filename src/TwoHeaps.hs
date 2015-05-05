{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- This file was copied wholesale from Heap.hs and then edited down to
-- the TwoHeaps demo.  Not my preferred method of operation, but I am
-- under a time-crunch at the moment!

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Monoid
import Text.Read (readMaybe)
import System.Random
import qualified Data.Map as M
import qualified Data.List as L

import Hyper.Canvas

import Hyper.Trees
import Hyper.TreesExtra

randRange = (10,99) :: (Int,Int)

data Config = Config { useAnimations :: Bool
                     , defaultTreeSize :: Int
                     , treeSizeInputID :: String
                     , newGameButtonID :: String
                     , freezeFrameDelay :: Int
                     , animationFrameDelay :: Int }

main = do let n = "main"
              s = "background: lightgray;"
          conf <- Config
                  <$> option n "use-animations" True
                  <*> option n "default-tree-size" 8
                  <*> option n "tree-size-input-id" "numnodes"
                  <*> option n "new-game-button-id" "restart"
                  <*> option n "freeze-frame-delay" 1000
                  <*> option n "animation-frame-delay" 100
          scUp <- startCanvas "upheap-tree"
                              (900,300) 
                              s
                              ["main","message","frame","defbuttons","score"]
                              (const (return ()))
          scDown <- startCanvas "downheap-tree"
                                (900,300)
                                s
                                ["main","message","frame","defbuttons","score"]
                                (const (return ()))
          startHeapGames (scUp,scDown,conf)

type StateModifier = GameState2 -> Writer [IO ()] GameState2

data GameState2 = Construction Int (ZTree (QNode HeapNode NodeStatus))
                | GameOver2 Int Bool

data NodeStatus = CurrentElem
                | Free
                | Frozen
                deriving (Read,Show,Eq,Ord)

newNode g = (fst $ randomR randRange g, True)

carelessInsert2 :: Ord a => a -> ZTree a -> ZTree a
carelessInsert2 a zt = (ztReplace a
                        . bottom
                        . Heap
                        . zTree
                        . ztUpMost) zt

newFakeHeap :: Ord a => [a] -> ZTree a
newFakeHeap = foldr carelessInsert2 (ZTree EmptyTree Top)

freeNextNodeUp :: Env -> StateModifier
freeNextNodeUp env (Construction n zt) = 
  tell [writeState env state] >> return state
  where state = if frozenNodes zt
                   then (Construction n . focus . go . unFocus) (ztUpMost zt) 
                   else Construction n zt
        go zt = bottomBy (qIs Frozen) (zTree zt)

focus :: ZTree (QNode a NodeStatus) -> ZTree (QNode a NodeStatus)
focus (ZTree (BiNode l (QNode n _) r) c) = 
  ZTree (BiNode l (QNode n CurrentElem) r) c

unFocus :: ZTree (QNode a NodeStatus) -> ZTree (QNode a NodeStatus)
unFocus = fmap (\q -> case q of
                        QNode n CurrentElem -> QNode n Free
                        q -> q)

ztIs :: NodeStatus -> ZTree (QNode a NodeStatus) -> Bool
ztIs s (ZTree (BiNode _ (QNode _ q) _) _) = s == q

frozenNodes :: ZTree (QNode a NodeStatus) -> Bool
frozenNodes = or . zSequence . fmap (qIs Frozen)

qIs :: Eq b => b -> QNode a b -> Bool
qIs s (QNode _ q) = s == q

thaw :: ZTree (QNode a NodeStatus) -> ZTree (QNode a NodeStatus)
thaw (ZTree (BiNode l (QNode n _) r) c) = 
  ZTree (BiNode l (QNode n Free) r) c

freeNextNodeDown :: Env -> StateModifier
freeNextNodeDown env (Construction n zt) = 
  tell [writeState env state] >> return state
  where state = if frozenNodes zt
                   then (Construction n . focus . go . unFocus) (ztUpMost zt) 
                   else Construction n zt
        go zt = lastElemBy (not . qIs Frozen) (zTree zt)

modSwap :: Env 
        -> ( ZTree (QNode HeapNode NodeStatus) 
          -> ZTree (QNode HeapNode NodeStatus) ) 
        -> StateModifier
modSwap env mod (Construction n t) = 
  let state = Construction (n + 1) (mod t)
  in tell [writeState env state] >> return state
modSwap _ _ s = return s

modValidate :: Env -> StateModifier
modValidate env (Construction n zt) = 
  let et = EditTree zt
      (m,trace) = validateM env et
  in tell [visualize env 100 1000 "Validating..." trace] 
     >> case m of
          True -> let state = GameOver2 n True
                  in tell [writeState env state] >> return state
          _ -> let state = GameOver2 n False
               in tell [writeState env state] >> return state

data Env = Env { sc :: HyperCanvas
               , conf :: Config
               , runM :: StateModifier -> IO ()
               , modFreeNext :: Env -> StateModifier
               , getNodeForm :: Env -> NodeForm (QNode HeapNode NodeStatus)
               , title :: String
               , moveName :: String }

nodesize = (30,40)


startHeapGames :: (HyperCanvas,HyperCanvas,Config) -> IO ()
startHeapGames (scUp,scDown,conf) = 
  do g <- newStdGen
     (gameManipsU,runManipU) <- newAddHandler
     (gameManipsD,runManipD) <- newAddHandler
     let runManip a = runManipU a >> runManipD a
         envUp = Env scUp 
                     conf 
                     runManipU 
                     freeNextNodeUp 
                     upNodeForm 
                     ":: Upheap Construction ::"
                     "Upheap"
         envDown = Env scDown 
                       conf 
                       runManipD 
                       freeNextNodeDown 
                       downNodeForm
                       ":: Downheap Construction ::"
                       "Downheap"
     attachButton (newGameButtonID conf) 
                  (do g <- newStdGen
                      doNewGame envUp g
                      doNewGame envDown g) 
                  (const return ())
     startGame envUp gameManipsU g
     startGame envDown gameManipsD g

doNewGame env g = do n <- restartGame env <$> readNewGame env g
                     (runM env) n

startGame env gameManips g = 
  do initialGame <- readNewGame env g
     compile (heapGame env
                       (initialGame, [writeState env initialGame])
                       gameManips
                       (runM env)) >>= actuate
     (runM env) ((modFreeNext env) env)
     writeS (sc env) "frame" (combine [mkTitle env (title env), frameForm])

frameForm = rekt (15,15) (870,284) False Black

restartGame :: Env -> GameState2 -> StateModifier
restartGame env newGame _ = ((modFreeNext env) env newGame)
--   let (newGame',frames) = runWriter 
--   in tell [frames] 
--      >> return newGame' 

readNewGame :: Env -> StdGen -> IO GameState2
readNewGame env g = 
  do num <- safeReadInput (treeSizeInputID (conf env)) 
                          (defaultTreeSize (conf env)) 
     let heap = (newFakeHeap 
                 . take num 
                 . fmap (makeQ Frozen)
                 . fmap HeapNode 
                 . randomRs randRange) g
     changeInput (treeSizeInputID (conf env)) (show num)
     return (Construction 0 heap)

heapGame env iGame gameMs runM = 
  do eGameMs <- fromAddHandler gameMs
     let gstate = fst <$> bGameM
         vstate = snd <$> bGameM
         bGameM =  accumB iGame (update <$> eGameMs)
     visuals <- changes vstate
     reactimate' (fmap sequence_ <$> visuals)
     return ()

update :: StateModifier -> (GameState2,[IO ()]) -> (GameState2,[IO ()])
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
  in dumbButtons env
     >> if (useAnimations . conf) env
           then (sequence_ . fmap (w d)) vt
                >> messageHold env White (d * length vt) str
                >> w finald (L.last vt)
           else w finald (L.last vt)

writeState :: Env -> GameState2 -> IO ()
writeState env (Construction n zt) = 
  do let tree = fitTreeArea env (toForm nodesize
                                        zFindLoc
                                        ((getNodeForm env) env)
                                        (zTree (ztUpMost zt)))
         doFreeNext = (runM env) ((modFreeNext env) env)
         bFreeNext = (fitControl1 env . addOnClick [doFreeNext]) 
                       (buttonForm "Next Node" LightBlue)
         doValidate = (runM env) (modValidate env)
         bValidate = (fitControl2 env . addOnClick [doValidate]) 
                       (buttonForm "Validate" Orange)
     (write (sc env) "main" . combine) [tree,bFreeNext,bValidate]
     >> message env LightYellow "Construct the heap, moving one node at a time."
     >> (writeS (sc env)) "score" (scoreBox env n)
writeState env (GameOver2 n b) = 
  if b
     then message env LightGreen "The heap is valid."
     else message env LightRed "Invalid heap! Violations are marked in red." 

modRemoveMin = undefined
modInsertNew = undefined
writeEditState = undefined

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

downNodeForm :: Env -> NodeForm (QNode HeapNode NodeStatus)
downNodeForm env zt = 
  let (form,line) = nodeForm zt
  in case zt of
       ZTree _ (L (QNode _ CurrentElem) _ _) -> 
         (combine [addOnClick [(runM env) (modSwap env (downHeapLZ id))] form
                  ,highlight env]
         ,line)
       ZTree _ (R _ (QNode _ CurrentElem) _) -> 
         (combine [addOnClick [(runM env) (modSwap env (downHeapRZ id))] form
                  ,highlight env]
         ,line)
       _ -> (form,line)

upNodeForm :: Env -> NodeForm (QNode HeapNode NodeStatus)
upNodeForm env zt = 
  let (form,line) = nodeForm zt
  in case zt of
       ZTree (BiNode (BiNode _ (QNode _ CurrentElem) _) _ _) _ -> 
         (combine [addOnClick [(runM env) (modSwap env (upHeapZ id))] form
                  ,highlight env]
         ,line)
       ZTree (BiNode _ _ (BiNode _ (QNode _ CurrentElem) _)) _ -> 
         (combine [addOnClick [(runM env) (modSwap env (upHeapZ id))] form
                  ,highlight env]
         ,line)
       _ -> (form,line)

dumbButtons :: Env -> IO ()
dumbButtons env = 
  writeS (sc env) 
         "defbuttons" 
         (combine [fitControl1 env (buttonForm "" Gray)
                  ,fitControl2 env (buttonForm "" Gray)])

clearDumbButtons env = writeS (sc env) "defbuttons" blank

messageForm :: Color -> String -> HyperForm
messageForm col str = combine [rekt (0,0) (500,30) True col
                              ,rekt (0,0) (500,30) False Black
                              ,text (250,15) (490,15) str]

buttonForm :: String -> Color -> HyperForm
buttonForm str col = combine [rekt (1,1) (160,30) False Black
                             ,rekt (0,0) (160,30) True col
                             ,rekt (0,0) (160,30) False Black
                             ,text (80,15) (155,17) str]

fitTreeArea :: Env -> HyperForm -> HyperForm
fitTreeArea env sf = let treeAreaX = 810
                         ff = fit (45,134) (810,150) sf
                         xv = (fst (snd (bounds ff))) / 2
                     in translate (treeAreaX / 2 - xv + 8,0) ff


mkTitle :: Env -> String -> HyperForm
mkTitle env s = translate (45,35) (combine [rekt (0,0) (315,30) True White
                                           ,rekt (0,0) (315,30) False Black
                                           ,text (157,15) (315,15) s])

scoreBox :: Env -> Int -> HyperForm
scoreBox env n = translate (600,75) (combine [rekt (0,0) (255,30) True White
                                             ,rekt (0,0) (255,30) False Black
                                             ,text (128,15) (255,15) s])
  where s = (moveName env) ++ "s used: " ++ show n

fitControl1 :: Env -> HyperForm -> HyperForm
fitControl1 env = fit (45,75) (150,100)

fitControl2 :: Env -> HyperForm -> HyperForm
fitControl2 env = fit (210,75) (150,100)

fitMessageArea :: Env -> HyperForm -> HyperForm
fitMessageArea env = fit (375,35) (480,100)

data HeapNode = HeapNode Int deriving (Show, Eq, Ord)

instance DrawableNode HeapNode where
  nodeForm (HeapNode v) = (text (0,0) (200,100) (show v)
                          ,(\ploc -> line (0,0) ploc 2 Black))

data QNode a s = QNode { qVal :: a
                       , qStatus :: s } deriving (Show)

instance DrawableNode (QNode HeapNode NodeStatus) where
  nodeForm (QNode h CurrentElem) = qForm h Orange
  nodeForm (QNode h Free) = qForm h White
  nodeForm (QNode h Frozen) = qForm h Gray

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
  nodeForm (QNode h BadBoth) = nodeForm (QNode h BadChild)

qForm :: HeapNode -> Color -> (HyperForm, LineForm) 
qForm h c = let (t,line) = nodeForm h
                r = rekt (-150,-100) (300,200) True c
                o = rekt (-150,-100) (300,200) False Black
            in (combine [r,o,t], line) 

highlight :: Env -> HyperForm
highlight env = combine [circle (-143,-100) 48 True Orange]

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
            | BadBoth
            | BadParent deriving (Show)

setQ :: q -> QNode a s -> QNode a q
setQ q (QNode n _) = QNode n q

makeQ :: q -> a -> QNode a q
makeQ q a = QNode a q 

stamp :: Status -> ZTree (QNode a Status) -> ZTree (QNode a Status)
stamp BadParent (ZTree (BiNode l (QNode v BadChild) r) c) = 
  ZTree (BiNode l (QNode v BadBoth) r) c
stamp s (ZTree (BiNode l (QNode v _) r) c) = 
  ZTree (BiNode l (QNode v s) r) c
stamp _ t = t

type Validator a = StateT Bool (Writer (VTrace a Status)) (ZTree (QNode a Status))

type VTrace a b = [ZTree (QNode a b)]

validateM :: Ord a 
          => Env 
          -> (EditTree (QNode a NodeStatus)) 
          -> (Bool, VTrace a Status)
validateM env (EditTree t) = 
  case nt of
    (ZTree (BiNode _ _ _) _) -> 
      let ((res,win),trace) = runWriter (runStateT (markValid nt >>= check) True)
      in (win
         ,trace)
    _ -> (True, [])
  where nt = (fmap (setQ Unchecked) . ztUpMost) t

passGood True = True
passGood b = b

passBad _ = False

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
markFail zt = let 
                  nxt = (stamp BadParent 
                         . ztUp 
                         . stamp BadChild) zt
              in tell [nxt] >> modify passBad >> return nxt
