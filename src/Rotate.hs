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

data Config = Config { defaultTreeSize :: Int
                     , useStopwatch :: Bool }

prep = (,) 
       <$> startCanvas "thecanvas"
       <*> (Config
            <$> option "default-tree-size" 16
            <*> option "use-stopwatch" True)

main = prep >>= treestuff

type ColorTree = BiTree (Bool, Color)
type GameState = (ColorTree, (SuperForm, ColorTree))
type OutputSet = (SuperForm, SuperForm)

treestuff (sc,conf) = 
  do t <- newAddHandler
     b <- newAddHandler
     attachButton "gen" (snd b)
     fd <- newAddHandler
     attachField "numnodes" (snd fd)
     seedH <- newAddHandler
     attachField "seed" (snd seedH)
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
                               (fst fd)
                               (fst seedH)
                               (fst tm)
                               (fst res)
                               (snd t)
                               (snd res))
     actuate network
     
     changeValue "tellseed" ((show.abs) rando)
     (snd t) (ref, (emptyForm, nxt))
     startTimer 1000000 (snd tm)
     putStrLn "Started?"
     return ()

mkNet (sc,conf) ref t button field seedH timeH resetH fire reset = 
  do eTrees <- fromAddHandler t
     eButton <- fromAddHandler button
     eField <- fromAddHandler field
     eSeed <- fromAddHandler seedH
     eTimer <- fromAddHandler timeH
     eResets <- fromAddHandler resetH
     let bTime = accumB timeAmount 
                        ((timeUpd <$ eTimer)
                              `union` (timeRes <$ eResets))
         eNums = fmap (tryread (defaultTreeSize conf)) eField
         bNum = stepper (defaultTreeSize conf)  eNums
         eSeedNums = fmap (tryread 0) eSeed
         bSeedNums = stepper 0 eSeedNums
         bGenInfo = (,) <$> bNum <*> bSeedNums
         bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format fire) eTrees 
         eForms = eTreeForms
         bTrees = stepper (EmptyTree, (emptyForm, EmptyTree)) eTrees
     timerC <- changes ((,) <$> bTime <*> bTrees)
     reactimate' (fmap (evalState sc) <$> timerC)
     reactimate (fmap (display sc) eForms)
     reactimate (fmap (newtrees reset fire) (bGenInfo <@ eButton))

timeRes _ = timeAmount

evalState :: SuperCanvas -> (Int, GameState) -> IO ()
evalState sc (t,g) = do changeValue "timer" (show t)
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

newtrees res t (n,r) = do g <- newStdGen
                          res ()
                          let (rando) = if r == 0
                                           then (abs.fst) (random g) 
                                           else r
                              (ref,nxt) = randomColorTrees n rando
                          changeValue "tellseed" (show rando)
                          t (ref,(emptyForm,nxt))

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n

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

