import Reactive.Banana hiding (split,now)
import Reactive.Banana.Frameworks

import Debug.Trace (trace)

import Text.Read (readMaybe)
import System.Random
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L

import Super.Canvas
import Super.Trees

treeAreaSize = (800, 195)

minNodes = 3 :: Int

data Config = Config { canvasWidth :: Double
                     , canvasHeight :: Double
                     , defaultTreeSize :: Int
                     , maximumTreeSize :: Int
                     , useStopwatch :: Bool
                     , canvasStyle :: String
                     , treeSizeInputID :: String
                     , seedInputID :: String
                     , newGameButtonID :: String}

prep :: IO (SuperCanvas, Config, AddHandler Double)
prep = do let n = "main"
              s = "background: lightgray;"
          conf <- Config
                  <$> option n "canvas-width" 900
                  <*> option n "canvas-height" 500
                  <*> option n "default-tree-size" 16
                  <*> option n "maximum-tree-size" 99
                  <*> option n "use-stopwatch" True
                  <*> option n "canvas-style" s
                  <*> option n "tree-size-input-id" "numnodes"
                  <*> option n "seed-input-id" "seed"
                  <*> option n "new-game-button-id" "newgame"
          (clock,tick) <- newAddHandler
          sc <- startCanvas n 
                            ( canvasWidth conf
                            , canvasHeight conf )
                            (canvasStyle conf)
                            ["main","stopwatch"]
                            tick
          return (sc, conf, clock)

main = prep >>= treestuff

data NewGame = NewGame { ngNumNodes :: Int
                       , ngSeed :: Int     }

type Env = (Config, SuperCanvas, Handler TreeR)

readNewGame :: Env -> IO (GameState -> GameState)     
readNewGame (conf,sc,h) = 
  do defSeed <- (abs . fst . random) <$> newStdGen
     let defNodes = defaultTreeSize conf
         maxNodes = maximumTreeSize conf
     nn <- safeReadInput (treeSizeInputID conf) defNodes
     seed <- safeReadInput (seedInputID conf) defSeed
     let numNodes = max minNodes (min maxNodes nn)
     changeInput (treeSizeInputID conf) (show numNodes)
     changeInput (seedInputID conf) ("")
     return (newGameState (conf,sc,h) (NewGame numNodes seed))

data GameState = GameState { gsRefTree :: ColorTree
                           , gsWorkTree :: ColorTree
                           , gsForm :: SuperForm
                           , gsMoveCount :: Int }

genTrees :: NewGame -> (ColorTree, ColorTree)
genTrees ng = randomColorTrees (ngNumNodes ng) (ngSeed ng)

emptyState :: GameState
emptyState = GameState EmptyTree EmptyTree blank 0

newGameState :: Env -> NewGame -> GameState -> GameState
newGameState (conf,sc,h) ng _ = 
  let (ref,work) = genTrees ng
  in GameState ref work blank 0

initGame :: Env -> IO GameState
initGame (conf,sc,h) = 
  do state <- readNewGame (conf,sc,h) <*> pure emptyState
     render (conf,sc,h) state
     return state

treestuff (sc,conf,clock) = 
  do t <- newAddHandler -- for trees to return rotations
     b <- newAddHandler -- for button clicks that restart the game
     (tocks,doTock) <- newAddHandler
     let h = snd t
     attachButton (newGameButtonID conf) 
                  (readNewGame (conf,sc,h)) 
                  (snd b)
     iState <- initGame (conf,sc,h)
     
     net2 <- compile (timeNet clock doTock 0 (fst b))

     network <- compile (mkNet (conf,sc,h)
                               iState 
                               (fst t) 
                               (fst b)
                               clock
                               (tocks,doTock))
     actuate net2
     actuate network
     -- putStrLn "Started?"

data TimeBuf = TimeBuf Double Bool

timeNet clock doTock startTime restarts = 
  do eClock <- fromAddHandler clock
     eRestarts <- fromAddHandler restarts
     let bSyncTime = stepper (setTime startTime) (setTime <$> eClock)
         eTimeUpdates = (bSyncTime <@ eRestarts) 
                        `union` (discrete 1000 <$> eClock)
         bTimeBuf = accumB (TimeBuf startTime False) eTimeUpdates
     tc <- changes bTimeBuf
     reactimate' (fmap (\(TimeBuf t tock) -> if tock
                                                then doTock () 
                                                else return ()) <$> tc)

setTime :: Double -> TimeBuf -> TimeBuf
setTime t _ = TimeBuf t False

discrete :: Double -> Double -> TimeBuf -> TimeBuf
discrete thresh newT (TimeBuf oldT _) = 
  if (newT - oldT) > thresh
     then TimeBuf newT True
     else TimeBuf oldT False

mkNet (conf,sc,h) iState treeRs newGames clock (tocks,doTock) = 
  do eRotations <- fromAddHandler treeRs 
     eNewGames <- fromAddHandler newGames 
     eTocks <- fromAddHandler tocks
     let bStopWatch = accumB 0 ((const 0 <$ eNewGames)
                                `union` (bWin (+1) <@ eTocks))
         eTreeUps = fmap (treeUp (conf,sc,h)) eRotations
         bState = accumB iState (eNewGames `union` eTreeUps)
         bWin a = (\s -> if not (complete s)
                            then a
                            else id) <$> bState
     watchChanges <- changes bStopWatch
     reactimate' (fmap (rwatch (conf,sc,h)) <$> watchChanges)
     stateChanges <- changes bState
     reactimate' (fmap (render (conf,sc,h)) <$> stateChanges)

complete :: GameState -> Bool
complete gs = gsRefTree gs == gsWorkTree gs

treeUp :: Env -> TreeR -> GameState -> GameState
treeUp (conf,sc,h) tr gs = GameState (gsRefTree gs) 
                                     (trTree tr) 
                                     (trForm tr) 
                                     (gsMoveCount gs + 1)

render :: Env -> GameState -> IO ()
render (conf,sc,h) gs = 
  (sequence_ . fmap (animate sc "main" 5 42)) (format (conf,sc,h) gs)

format :: Env -> GameState -> [SuperForm]
format (conf,sc,h) gs =
  let (fitRef, fitWork, fitMoves, fitWin, fitTime) = 
        layouts (conf,sc,h)
      ref = gsRefTree gs
      work = gsWorkTree gs
      win = translate (100,25)
                      (text (0,0)
                            (250,200)
                            ("Complete!"))
      mc = translate (100,25) 
                     (text (0,0) 
                           (200,100) 
                           ("Moves: " ++ show (gsMoveCount gs)))
  in [ combine [ fitRef (prepSTree ref)
               , fitMoves mc
               , fitWork (gsForm gs) ]
     , combine ([ fitRef (prepSTree ref)
                , fitMoves mc ]
                ++ (if complete gs -- win-state!
                       then [fitWork (prepSTree work)
                            ,fitWin win]
                       else [fitWork (prepTree h work)])) ]

rwatch :: Env -> Int -> IO ()
rwatch (conf,sc,h) t = writeS sc "stopwatch" (rformat (conf,sc,h) t)

rformat :: Env -> Int -> SuperForm
rformat (conf,sc,h) t = let (_,_,_,_,fitTime) = layouts (conf,sc,h)
                        in fitTime (translate (100,25)
                                              (text (0,0)
                                                    (250,200)
                                                    (show t)))

layouts (conf,sc,h) = 
  let padding = 30 :: Double
      toTup x = (x,x)
      treeAreaX = canvasWidth conf * 2 / 3 - padding * 2
      treeAreaY = canvasHeight conf / 2 - padding * 2
      treeBox = (treeAreaX, treeAreaY)
      
      infoX = canvasWidth conf * 1 / 3 - padding * 2
      infoY = canvasHeight conf / 3 - padding * 2
      infoBox = (infoX, infoY)

      fitRef = fit (toTup padding) treeBox
      fitWork = fit (padding, padding * 2 + treeAreaY) treeBox
      fitMoves = fit (padding * 2 + treeAreaX, padding) infoBox
      fitWin = fit (padding * 2 + treeAreaX
                   ,padding * 2 + infoY) infoBox
                   
      fitTime = fit (padding * 2 + treeAreaX
                    ,padding * 3 + infoY * 2) infoBox

  in (fitRef, fitWork, fitMoves, fitWin, fitTime)

randomColorTrees :: Int -> Int -> (ColorTree, ColorTree)
randomColorTrees i r = 
  let g1 = mkStdGen r
      (g2,g3) = split g1
      nodes = take i (zip (repeat True) 
                          (randomRs (Red,Yellow) g1))
      tree = randomTree nodes 
  in (tree g2, tree g3)

