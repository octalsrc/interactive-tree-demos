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
     
     return (prepTree (snd t) (sampleTree 4 Red)) >>= (snd t)
     putStrLn "Started?"

     return ()

off = (450, 50)

mkNet c t fire = 
  do eClicks <- fromAddHandler c 
     eTrees <- fromAddHandler t 
     let ePlates = eTrees -- fmap (\t -> prepTree fire t) eTrees
         eActions = fmap (\as cs -> Prelude.filter (checkB cs off) as) 
                         (fmap (onlyClicks . getActions) ePlates)
         bLive = stepper (const []) eActions
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     reactimate (fmap sequence_ eTriggered) 
     reactimate (fmap (drawPlate off) ePlates)

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
