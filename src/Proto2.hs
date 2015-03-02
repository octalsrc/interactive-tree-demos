import Reactive.Banana
import Reactive.Banana.Frameworks

import Super.Canvas.Trees
import Super.Canvas.Types
import Super.Canvas.JS


main = 
  do putStrLn "Starting..."
     c <- newAddHandler
     t <- newAddHandler 
     attachHandlers (snd c)
     network <- compile (mkNet (fst c) (fst t) (snd t))
     actuate network
     
     return ([], sampleTree 5 Red) >>= (snd t)
     putStrLn "Started?"

     return ()

off = (50, 50)

mkNet c t fire = 
  do eClicks <- fromAddHandler c
     eTrees <- fromAddHandler t
     let ePlates = fmap (\(tv,bt) -> (tv, prepTree fire bt)) eTrees
         eActions = fmap (\as cs -> Prelude.filter (checkB cs off) as) 
                         (fmap (onlyClicks . getActions . snd) ePlates)
         bLive = stepper (const []) eActions
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     reactimate (fmap sequence_ eTriggered) 
     reactimate (fmap (\(tv,p) -> trya tv >> drawPlate off p) ePlates)

trya [] = return ()
trya tv = animate 2000 4 (fmap (\(p,s,d) -> (p
                                            ,(s + off)
                                            ,(d + off))) tv)

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
