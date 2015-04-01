import Reactive.Banana
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random

import Super.Canvas
import Super.Trees

main = startCanvas "thecanvas" >>= treestuff

type HeapTree = BiTree (Bool, Int)

treestuff sc = 
  do t <- newAddHandler
     g <- newStdGen
     b <- newAddHandler
     attachButton "newnode" (snd b)
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
     reactimate (fmap (write sc) eForms)


addNode' t g = addNode g t

addNode :: StdGen -> HeapTree -> HeapTree
addNode g = (\(a,_) -> a)
            . qtUpMost
            . insertNew (newNode g) g
            . (\a -> (a,Top))
            . clean

randRange = (10,99)

newNode g = (True, fst $ randomR randRange g)

clean :: HeapTree -> HeapTree
clean = fmap (\(b,v) -> (False,v))

randomHeapTree i g = 
  randomTree(fmap (\a -> (False,a)) (take i (randomRs randRange g))) g

insertNew node g qt = 
  ( (\(t,c) -> (BiNode EmptyTree node EmptyTree,c))
  . randomChild g ) qt

format :: SuperForm -> SuperForm
format = translate (50,50)

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n



format4 fire trees = fit (50,50) (800,400) (prepHeapTree fire trees)
