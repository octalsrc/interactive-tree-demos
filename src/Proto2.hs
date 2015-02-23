import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Default (def)

import GHCJS.Foreign
import GHCJS.Types
import JavaScript.JQuery
import JavaScript.Canvas hiding (rotate)
import Data.Text (pack, unpack)

import Text.Read (readMaybe)

import Super.Canvas.Trees
import Super.Canvas.Types
import Super.Canvas.JS


main = 
  do putStrLn "Starting..."
     c <- newAddHandler
     m <- newAddHandler
     i <- newAddHandler
     t <- newAddHandler
     can <- select (pack "#thecanvas") >>= indexArray 0 . castRef >>= getContext
     attachHandlers (snd c) (snd m)
     -- mkInput (snd i)
     network <- compile (mkNet can (fst c) (fst m) (fst i) (fst t) (snd t))
     actuate network
     
     return (sampleTree 4 Red) >>= (snd t)
     putStrLn "Started?"

     return ()

mkNet can c m i t fire = 
  do eClicks <- fromAddHandler c
     --eMoves <- fromAddHandler m
     -- eKeys <- fromAddHandler i
     eTrees <- fromAddHandler t 
     let -- eRotatos = fmap (\t -> (fst . qtUpMost . rotate) t) eTrees
         ePlates = fmap (\t -> prepTree' fire t (1,1)) eTrees
         eActions = fmap (\as cs -> Prelude.filter (checkB cs) as) 
                         (fmap (onlyClicks . getActions) ePlates)
         bLive = stepper (const []) eActions
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     reactimate (fmap sequence_ eTriggered) 
     reactimate (fmap drawPlate ePlates)

onlyClicks ((x,b,OnClick io) : as) = (x,b,OnClick io) : (onlyClicks as)
onlyClicks (_:as) = onlyClicks as
onlyClicks _ = []

asdf :: [QualAction] -> IO ()
asdf ((_,_,a) :as) = do --(t,f) <- actio a
                        --return (fst (rotate t)) >>= f
                        actio a >> asdf as
asdf _ = return ()

attachHandlers c m = do can <- select (pack "#thecanvas")
                        let h ev = c =<< getMousePos ev
                        click h def can
                        --let b ev = m =<< getMousePos ev
                        --mousemove b def can
                        return ()

checkB :: (Double, Double) -> QualAction -> Bool
checkB (x,y) ((a,b),(w,h),_) = x >= a - w / 2
                               && x <= (a + w / 2)
                               && y >= b - h / 2
                               && y <= (b + h / 2)
