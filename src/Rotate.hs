import Reactive.Banana hiding (split)
import Reactive.Banana.Frameworks

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

prep :: IO (SuperCanvas, Config)
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
          sc <- startCanvas n 
                            ( canvasWidth conf
                            , canvasHeight conf )
                            (canvasStyle conf)
          return (sc, conf)

main = prep >>= treestuff

data NewGame = NewGame { ngNumNodes :: Int
                       , ngSeed :: Int     }

readNewGame :: Config -> Handler TreeR -> IO (GameState -> GameState)     
readNewGame conf h = 
  do defSeed <- (abs . fst . random) <$> newStdGen
     let defNodes = defaultTreeSize conf
         maxNodes = maximumTreeSize conf
     nn <- safeReadInput (treeSizeInputID conf) defNodes
     seed <- safeReadInput (seedInputID conf) defSeed
     let numNodes = max minNodes (min maxNodes nn)
     changeInput (treeSizeInputID conf) (show numNodes)
     changeInput (seedInputID conf) ("")
     return (newGameState conf h (NewGame numNodes seed))

data GameState = GameState { gsRefTree :: ColorTree
                           , gsWorkTree :: ColorTree
                           , gsForms :: [SuperForm]  }

genTrees :: NewGame -> (ColorTree, ColorTree)
genTrees ng = randomColorTrees (ngNumNodes ng) (ngSeed ng)

emptyState :: GameState
emptyState = GameState EmptyTree EmptyTree []

newGameState :: Config -> Handler TreeR -> NewGame -> GameState -> GameState
newGameState conf h ng _ = 
  let (ref,work) = genTrees ng
  in GameState ref work (format conf h (TreeR work blank) ref)

initGame :: Config -> SuperCanvas -> Handler TreeR -> IO GameState
initGame conf sc h = do state <- readNewGame conf h <*> pure emptyState
                        render sc (gsForms state)
                        return state

treestuff (sc,conf) = 
  do t <- newAddHandler -- for trees to return rotations
     b <- newAddHandler -- for button clicks that restart the game
     attachButton (newGameButtonID conf) 
                  (readNewGame conf (snd t)) 
                  (snd b)  
     iState <- initGame conf sc (snd t)
     network <- compile (mkNet (sc,conf,iState) 
                               (fst t) 
                               (fst b) 
                               (snd t))
     actuate network
     putStrLn "Started?"

mkNet (sc,conf,iState) treeRs newGames fire = 
  do eRotations <- fromAddHandler treeRs 
     eNewGames <- fromAddHandler newGames 
     let eTreeUps = fmap (treeUp conf fire) eRotations
         bState = accumB iState (eNewGames `union` eTreeUps)
     stateChanges <- changes bState
     reactimate' (fmap (render sc . gsForms) <$> stateChanges)


treeUp :: Config -> Handler TreeR -> TreeR -> GameState -> GameState
treeUp conf fire tr (GameState ref work forms) = 
  GameState ref (trTree tr) (format conf fire tr ref)

render :: SuperCanvas -> [SuperForm] -> IO ()
render sc = sequence_ . fmap (animate sc 5 42)

format :: Config -> Handler TreeR -> TreeR -> ColorTree -> [SuperForm]
format conf fire tr ref =
  let padding = 30 :: Double
      toTup x = (x,x)
      treeAreaX = canvasWidth conf * 2 / 3 - padding * 2
      treeAreaY = canvasHeight conf / 2 - padding * 2
      treeBox = (treeAreaX, treeAreaY)
      sRef = fit (toTup padding) treeBox (prepSTree ref)
      sWork = fit (padding, padding * 2 + treeAreaY) 
                  treeBox
                  (prepTree fire (trTree tr))
      aWork = fit (padding, padding * 2 + treeAreaY)
                  treeBox
                  (trForm tr)
  in [ combine [sRef, aWork]
     , combine [sRef, sWork] ]

randomColorTrees :: Int -> Int -> (ColorTree, ColorTree)
randomColorTrees i r = 
  let g1 = mkStdGen r
      (g2,g3) = split g1
      nodes = take i (zip (repeat True) 
                          (randomRs (Red,Yellow) g1))
      tree = randomTree nodes 
  in (tree g2, tree g3)

