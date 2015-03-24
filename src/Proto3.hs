import Reactive.Banana
import Reactive.Banana.Frameworks

import System.Random

import Super.Canvas
import Super.Trees

main = startCanvas "thecanvas" >>= treestuff


testshapes = combine [ circle (200, 150) 10 True Blue 
                     , rekt (100, 300) (10,60) Green  ]

testshapes2 = translate (450,50) ((prepSTree . fst) 
                                    (randomTrees 3 89))
                                    
treestuff sc = 
  do t <- newAddHandler
     g <- newStdGen
     let (rando,g') = random g
         thetrees = randomTrees 4 rando
     network <- compile (mkNet sc
                               (fst thetrees) 
                               (fst t) 
                               (snd t))
     actuate network
     -- write sc (format (prepSTree (fst thetrees)))
     (snd t) thetrees
     putStrLn "Started?"
     return ()

mkNet sc ref t fire = 
  do eTrees <- fromAddHandler t
     let eTreeForms = 
           fmap (\(a,b) -> 
                   combine 
                     [ format (prepSTree a)
                     , format2 (prepTree (\b -> fire (ref,b)) b)]) 
                eTrees 
         eForms = eTreeForms
     reactimate (fmap (write sc) eForms)

format :: SuperForm -> SuperForm
format = translate (50,50)

format2 = translate (50, 300)
