import Reactive.Banana
import Reactive.Banana.Frameworks

import System.Random

import Super.Canvas.Trees
import Super.Canvas.Types
import Super.Canvas.JS


main = 
  do putStrLn "Starting..."
     c <- newAddHandler
     t <- newAddHandler 
     attachHandlers (snd c)
     
     
     g <- newStdGen
     let (rando,g') = random g
         thetrees = randomTrees 4 rando
     network <- compile (mkNet (fst thetrees) (fst c) (fst t) (snd t))
     actuate network
     drawPlate off (prepSTree (fst thetrees))
     return ([], (snd thetrees)) >>= (snd t)
     --return ([], sampleTree 5 Red) >>= (snd t)
     putStrLn "Started?"

     return ()

off = (50, 50)
off' = (50, 300)

mkNet reftree c t fire = 
  do eClicks <- fromAddHandler c
     eTrees <- fromAddHandler t
     let ePlates = fmap (\(tv,bt) -> (tv, prepTree fire bt)) eTrees
         eActions = fmap (\as cs -> Prelude.filter (checkB cs off') as) 
                         (fmap (onlyClicks . getActions . snd) ePlates)
         bLive = stepper (const []) eActions
         bWin = stepper (False) (fmap ((== reftree) . snd) eTrees)
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     cwin <- changes bWin
     reactimate (fmap sequence_ eTriggered) 
     reactimate (fmap (\(tv,p) -> (trya $! tv) >> drawPlateB off' p) ePlates)
     reactimate' (fmap (\w -> if w
                                 then printWin
                                 else return ()) <$> cwin)

trya [] = return ()
trya tv = animate 500 4 (fmap (\(p,s,d) -> (p
                                           ,(s + off')
                                           ,(d + off'))) tv)

onlyClicks = filter (\a -> case a of
                             (_,_,OnClick _) -> True
                             _ -> False)

checkB :: (Double, Double) -> (Double, Double) -> QualAction -> Bool
checkB (xi,yi) (xo,yo) ((a,b),(w,h),_) = 
  let x = xi - xo
      y = yi - yo
  in x >= a - w / 2
     && x <= (a + w / 2)
     && y >= b - h / 2
     && y <= (b + h / 2) 
