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
     
     return (sampleTree 4 Red) >>= (snd t)
     putStrLn "Started?"

     return ()

mkNet c t fire = 
  do eClicks <- fromAddHandler c 
     eTrees <- fromAddHandler t 
     let ePlates = fmap (\t -> prepTree fire t) eTrees
         eActions = fmap (\as cs -> Prelude.filter (checkB cs) as) 
                         (fmap (onlyClicks . getActions) ePlates)
         bLive = stepper (const []) eActions
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     reactimate (fmap sequence_ eTriggered) 
     reactimate (fmap drawPlate ePlates)

onlyClicks = foldr (\(x,b,a) as -> case a of
                                     OnClick _ -> (x,b,a):as
                                     _         ->         as) []


checkB :: (Double, Double) -> QualAction -> Bool
checkB (x,y) ((a,b),(w,h),_) = x >= a - w / 2
                               && x <= (a + w / 2)
                               && y >= b - h / 2
                               && y <= (b + h / 2)
