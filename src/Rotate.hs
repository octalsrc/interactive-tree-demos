import Reactive.Banana hiding (split)
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L

import Super.Canvas
import Super.Trees

timeAmount = 60 :: Int

treeAreaSize = (800, 195)

minNodes = 3 :: Int

data Config = Config { defaultTreeSize :: Int
                     , maximumTreeSize :: Int
                     , useStopwatch :: Bool }

prep = (,) 
       <$> startCanvas "thecanvas"
       <*> (Config
            <$> option "default-tree-size" 16
            <*> option "maximum-tree-size" 99
            <*> option "use-stopwatch" True)

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
     tm <- newAddHandler
     res <- newAddHandler
     g <- newStdGen
     let (rando,g') = random g
         (ref,nxt) = randomColorTrees 
                       (defaultTreeSize conf) 
                       (abs rando)
     network <- compile (mkNet (sc,conf)
                               ref 
                               (fst t)
                               (fst b)
                               (fst tm)
                               (fst res)
                               (snd t)
                               (snd res))
     actuate network
     
     changeElem "tellseed" ((show.abs) rando)
     (snd t) (ref, (blank, nxt))
     startTimer 1000000 (snd tm)
     putStrLn "Started?"
     return ()

mkNet (sc,conf) ref t newGames timeH resetH fire reset = 
  do eTrees <- fromAddHandler t
     eNewGames <- fromAddHandler newGames
     eTimer <- fromAddHandler timeH
     eResets <- fromAddHandler resetH
     let bTime = accumB timeAmount 
                        ((timeUpd <$ eTimer)
                              `union` (timeRes <$ eResets))
         bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format fire) eTrees 
         eForms = eTreeForms
         bTrees = stepper (EmptyTree, (blank, EmptyTree)) eTrees
     timerC <- changes ((,) <$> bTime <*> bTrees)
     reactimate' (fmap (evalState sc) <$> timerC)
     reactimate (fmap (display sc) eForms)
     reactimate (fmap (newtrees reset fire) eNewGames)

timeRes _ = timeAmount

evalState :: SuperCanvas -> (Int, GameState) -> IO ()
evalState sc (t,g) = do changeElem "timer" (show t)
                        if t <= 0
                           then (write sc 
                                 . addGameOver
                                 . snd
                                 . format (\_ -> return ())) g
                           else return ()

addGameOver :: SuperForm -> SuperForm
addGameOver sf = combine [ text (500,300)
                                (200,150)
                                "Game Over"
                         , sf ]

display :: SuperCanvas -> OutputSet -> IO ()
display sc (as,s) = do if isBlank as
                          then return ()
                          else animate sc 5 (42) as 
                       write sc s

timeUpd t = if t <= 0
               then t
               else t - 1

newtrees res t ng = 
  do g <- newStdGen
     res ()
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

