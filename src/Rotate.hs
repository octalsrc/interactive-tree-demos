import Reactive.Banana hiding (split)
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random

import Super.Canvas
import Super.Trees

{- Config -}

defaultNodeCount = 12

treeAreaSize = (800, 195)

main = startCanvas "thecanvas" >>= treestuff

type ColorTree = BiTree (Bool, Color)
type GameState = (ColorTree, (TravelGroup, ColorTree))
type OutputSet = ([SuperForm], SuperForm)

treestuff sc = 
  do t <- newAddHandler
     b <- newAddHandler
     attachButton "gen" (snd b)
     fd <- newAddHandler
     attachField "numnodes" (snd fd)
     g <- newStdGen
     let (rando,g') = random g
         (ref,nxt) = randomColorTrees 16 rando
     network <- compile (mkNet sc
                               ref 
                               (fst t)
                               (fst b)
                               (fst fd)
                               (snd t))
     actuate network
     (snd t) (ref, ([], nxt))
     putStrLn "Started?"
     return ()

mkNet sc ref t button field fire = 
  do eTrees <- fromAddHandler t
     eButton <- fromAddHandler button
     eField <- fromAddHandler field
     let eNums = fmap (tryread defaultNodeCount) eField
         bNum = stepper defaultNodeCount eNums
         bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format fire) eTrees 
         eForms = eTreeForms
     reactimate (fmap (display sc) eForms)
     reactimate (fmap (newtrees fire) (bNum <@ eButton))

display :: SuperCanvas -> OutputSet -> IO ()
display sc (as,s) = animate sc (75 * 1000) as >> write sc s

newtrees t n = do g <- newStdGen
                  let (rando,_) = random g
                      (ref,nxt) = randomColorTrees n rando
                  t (ref,([],nxt))

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n

format :: Handler GameState -> GameState -> OutputSet
format fire (ref, (ani, nxt)) = 
  let pRef = fit (50,20) treeAreaSize (prepSTree ref)
      pAni = fmap (fit (50,300) treeAreaSize) (travel 4 ani)
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
      animates = fmap (\a -> combine [a,pRef]) pAni
  in (animates, nextshow)

randomColorTrees :: Int -> Int -> (ColorTree, ColorTree)
randomColorTrees i r = 
  let g1 = mkStdGen r
      (g2,g3) = split g1
      nodes = take i (zip (repeat True) (randoms g1))
      tree = randomTree nodes 
  in (tree g2, tree g3)
