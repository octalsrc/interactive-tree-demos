import Reactive.Banana
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random

import Super.Canvas
import Super.Trees

main = startCanvas "thecanvas" >>= treestuff

treestuff sc = 
  do t <- newAddHandler   
     let thetrees :: BiTree Int
         thetrees = sampleHeapTree 4 5
     network <- compile (mkNet sc
                               (fst t)
                               (snd t))
     actuate network
    -- write sc (format (prepSTree (fst thetrees)))
     (snd t) thetrees
     putStrLn "Started?"
     return ()

mkNet sc t fire = 
  do eTrees <- fromAddHandler t  
     let eTreeForms = fmap (format4 fire) eTrees 
         eForms = eTreeForms
     reactimate (fmap (write sc) eForms)

format :: SuperForm -> SuperForm
format = translate (50,50)

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n



format4 fire trees = translate (50,50) (prepHeapTree fire trees)
