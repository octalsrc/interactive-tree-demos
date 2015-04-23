module Super.Canvas ( circle
                    , fit
                    , line
                    , text
                    , rekt
                    , blank
                    , isBlank
                    , combine
                    , Location
                    , Factor
                    , Vector
                    , BoundingBox 
                    , scale
                    , onClick
                    , addOnClick
                    , translate
                    , travel
                    , getCanvas
                    , write
                    , idFactor
                    , idLocation
                    , idVector
                    , animate
                    , readElem
                    , safeReadElem
                    , changeInput
                    , readInput
                    , safeReadInput
                    , changeElem
                    , attachButton
                    , attachField
                    , Color (..)
                    , SuperCanvas
                    , option
                    , SuperForm
                    , startCanvas ) where

import Control.Event.Handler (Handler)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Queue
import Text.Read (readMaybe)

import qualified Data.Map as M

import Super.Canvas.Types
import Super.Canvas.JS

data SuperCanvas = SC { scContext :: Context
                      , scHandler :: (Handler [QualAction]) 
                      , scCState  :: CState                 }

safeReadElem :: Read a => String -> a -> IO a
safeReadElem n d = tryread d <$> readElem n

safeReadInput :: Read a => String -> a -> IO a
safeReadInput n d = tryread d <$> readInput n

option :: Read a => String -> a -> IO a
option n d = safeReadElem ("sc-option-" ++ n) d

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n

travel :: Vector -> SuperForm -> SuperForm
travel v = Leaf . Travel v

write :: SuperCanvas -> SuperForm -> IO ()
write c sf = animate c 1 0 sf

animate :: SuperCanvas -> Int -> Int -> SuperForm -> IO ()
animate (SC cx newAc t) numFrames delay sf = 
  do newAc (actions sf)
     writeToCanvas t cx delay (draws numFrames sf)

startCanvas :: String -> IO (SuperCanvas)
startCanvas name = 
  do can <- getCanvas name
     cH <- newAddHandler
     aH <- newAddHandler
     t <- initCState 
     attachClickHandler name (snd cH)
     network <- compile (mkNet (fst cH) (fst aH))
     actuate network
     return (SC can (snd aH) t)

mkNet c a = 
  do eClicks <- fromAddHandler c
     eQActions <- fromAddHandler a
     let arm = (\qa cs -> filter (checkB cs) qa)
         eActive = fmap arm eQActions
         bLive = stepper (const []) eActive
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     
     reactimate (fmap sequence_ eTriggered)

checkB :: Location -> QualAction -> Bool
checkB (x,y) ((a,b),(w,h),_) = x >= a
                               && x <= (a + w)
                               && y >= b
                               && y <= (b + h) 

blank :: SuperForm
blank = Node []

isBlank (Node []) = True
isBlank _ = False

fit :: Location -> BoundingBox -> SuperForm -> SuperForm
fit l (w,h) s = let ((sx,sy),(sw,sh)) = bounds s 
                    nf = minimum [ 1 -- never scale up
                                 , (w / (sw + sx))
                                 , (h / (sh + sy))] 
                in (translate (l) . scale (nf,nf)) s

circle :: Location -> Double -> Bool -> Color -> SuperForm
circle loc rad fill col = primElev loc 
                                   (loc - (rad,rad)) 
                                   (rad * 2, rad * 2) 
                                   (Circle rad fill col)

line :: Location -> Vector -> Double -> SuperForm
line loc dest thick = 
  let minx = min (fst loc) (fst (loc + dest))
      miny = min (snd loc) (snd (loc + dest))
      maxx = max (fst loc) (fst (loc + dest))
      maxy = max (snd loc) (snd (loc + dest))
      box = (maxx, maxy) - (minx, miny)
  in primElev loc (minx, miny) box (Line dest thick)

text :: Location -> BoundingBox -> String -> SuperForm
text loc box str = 
  primElev loc 
           (loc - (box / (2,2))) 
           box 
           (Text box str)

rekt :: Location -> BoundingBox -> Color -> SuperForm
rekt loc box col = 
  primElev loc loc box (Rekt box col)

combine :: [SuperForm] -> SuperForm
combine = Node . fmap pullUp

-- the purpose here is to cull unnecessary node-nesting
pullUp :: SuperForm -> SuperForm
pullUp (Node ((Node n):[])) = Node n
pullUp n = n

scale :: Factor -> SuperForm -> SuperForm
scale f = Leaf . Scale f

translate :: Vector -> SuperForm -> SuperForm
translate v = Leaf . Trans v 

onClick :: [IO ()] -> Location -> BoundingBox -> SuperForm
onClick ios l b =
  Leaf (Trans l (Leaf (Elem (Trigger b (fmap OnClick ios)))))

addOnClick :: [IO ()] -> SuperForm -> SuperForm
addOnClick ios sc = 
  let b = bounds sc
      a = onClick ios (fst b) (snd b)
  in combine [sc, a]

primElev :: Location -> Location -> BoundingBox -> Primitive 
         -> SuperForm
primElev loc boxloc box prim = 
  Node [ Leaf (Trans loc (Leaf (Elem (Prim prim))))
       , Leaf (Trans boxloc (Leaf (Elem (Bounds box))))]
