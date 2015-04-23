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
                     , canvasStyle :: String  }

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
          sc <- startCanvas n 
                            ( canvasWidth conf
                            , canvasHeight conf )
                            (canvasStyle conf)
          return (sc, conf)

main = prep >>= treestuff

type ColorTree = BiTree (Bool, Color)
type GameState = (ColorTree, (SuperForm, ColorTree))
type OutputSet = (SuperForm, SuperForm)

data NewGame = NewGame { ngNumNodes :: Int
                       , ngSeed :: Int     }

readNewGame :: Config -> IO NewGame     
readNewGame conf = 
  do defSeed <- (abs . fst . random) 
                <$> newStdGen
     let defNodes = defaultTreeSize conf
         maxNodes = maximumTreeSize conf
     nn <- safeReadInput "numnodes" defNodes
     seed <- safeReadInput "seed" defSeed
     let numNodes = max minNodes (min maxNodes nn)
     changeInput "numnodes" (show numNodes)
     changeInput "seed" ("")
     return (NewGame numNodes seed)

treestuff (sc,conf) = 
  do t <- newAddHandler
     b <- newAddHandler
     attachButton "newGame" 
                  (readNewGame conf)
                  (snd b)  
     g <- newStdGen
     let (rando,g') = random g
         (ref,nxt) = randomColorTrees 
                       (defaultTreeSize conf) 
                       (abs rando)
     network <- compile (mkNet (sc,conf)
                               ref 
                               (fst t)
                               (fst b)
                               (snd t))
     actuate network
     
     changeElem "tellseed" ((show.abs) rando)
     (snd t) (ref, (blank, nxt))
     putStrLn "Started?"
     return ()

mkNet (sc,conf) ref t newGames fire = 
  do eTrees <- fromAddHandler t
     eNewGames <- fromAddHandler newGames 
     let bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format fire) eTrees 
         eForms = eTreeForms
         bTrees = stepper (EmptyTree, (blank, EmptyTree)) eTrees 
     reactimate (fmap (display sc) eForms)
     reactimate (fmap (newtrees fire) eNewGames)

display :: SuperCanvas -> OutputSet -> IO ()
display sc (as,s) = do if isBlank as
                          then return ()
                          else animate sc 5 (42) as 
                       write sc s

timeUpd t = if t <= 0
               then t
               else t - 1

newtrees t ng = 
  do g <- newStdGen
     let rando = ngSeed ng
         (ref,nxt) = randomColorTrees 
                       (ngNumNodes ng)
                       rando
     changeElem "tellseed" (show rando)
     t (ref,(blank,nxt))

format :: Handler GameState -> GameState -> OutputSet
format fire (ref, (ani, nxt)) = 
  let pRef = fit (50,20) treeAreaSize (prepSTree ref)
      pAni = fit (50,300) treeAreaSize ani
      pNxt = fit (50,300) 
                 treeAreaSize
                 (prepTree (\b -> fire (ref,b)) nxt)
      can = combine [ pRef, pNxt ]
      nextshow = if ref == nxt
                    then combine 
                           [ text (500,300) 
                                  (200,150) 
                                  "You Win!"
                           , can             ]
                    else can
      animates = combine [ pAni, pRef ]
  in (animates, nextshow)

randomColorTrees :: Int -> Int -> (ColorTree, ColorTree)
randomColorTrees i r = 
  let g1 = mkStdGen r
      (g2,g3) = split g1
      nodes = take i (zip (repeat True) 
                          (randomRs (Red,Yellow) g1))
      tree = randomTree nodes 
  in (tree g2, tree g3)

