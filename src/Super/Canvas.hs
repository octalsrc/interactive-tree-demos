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
                    , bounds
                    , write
                    , writeS
                    , idFactor
                    , idLocation
                    , idVector
                    , animate
                    , animateS
                    , readElem
                    , safeReadElem
                    , now
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
import Reactive.Banana hiding (now)
import Reactive.Banana.Frameworks
import Data.Queue
import Text.Read (readMaybe)

import qualified Data.Map as M

import Super.Canvas.Types
import Super.Canvas.JS
import Super.Canvas.Concurrent

data SuperCanvas = SC { scContext :: Context
                      , scHandler :: (Handler [QualAction]) 
                      , scCState  :: CState
                      , scSize    :: BoundingBox }

startCanvas :: String 
            -> BoundingBox 
            -> String
            -> [String]
            -> Handler Double
            -> IO (SuperCanvas)
startCanvas name size style chans clock = 
  do can <- insertCanvas name size style
  
     cH <- newAddHandler
     aH <- newAddHandler   
     attachClickHandler name (snd cH)
     network <- compile (mkNet (fst cH) (fst aH))
     actuate network
             
     startTime <- now
     cstate <- newCState chans
     let writer = writeToCanvas size can
     startBrowserPageRun (stepCState cstate writer 
                          >> tick clock startTime)

     return (SC can (snd aH) cstate size)

tick :: Handler Double -> Double -> IO ()
tick clock startTime = 
  (-) <$> now <*> pure startTime >>= clock

mkNet c a = 
  do eClicks <- fromAddHandler c
     eQActions <- fromAddHandler a
     let arm = (\qa cs -> filter (checkB cs) qa)
         eActive = fmap arm eQActions
         bLive = stepper (const []) eActive
         eTriggered = fmap (fmap getIO) (bLive <@> eClicks)
     reactimate (fmap sequence_ eTriggered)

safeReadElem :: Read a => String -> a -> IO a
safeReadElem n d = tryread d <$> readElem n

safeReadInput :: Read a => String -> a -> IO a
safeReadInput n d = tryread d <$> readInput n

option :: Read a => String -> String -> a -> IO a
option n o d = safeReadElem ("sc-" ++ n ++ "-option-" ++ o) d

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n

travel :: Vector -> SuperForm -> SuperForm
travel v = Leaf . Travel v

write :: SuperCanvas -> String -> SuperForm -> IO ()
write sc chan sf = animate sc chan 1 0 sf

writeS :: SuperCanvas -> String -> SuperForm -> IO ()
writeS sc chan sf = animateS sc chan 1 0 sf

animate :: SuperCanvas -> String -> Int -> Int -> SuperForm -> IO ()
animate sc chan numFrames delay sf = 
  (scHandler sc) (actions sf) 
  >> enqueueDraws (scCState sc) chan delay (draws numFrames sf)

animateS :: SuperCanvas -> String -> Int -> Int -> SuperForm -> IO ()
animateS sc chan numFrames delay sf = 
  enqueueDraws (scCState sc) chan delay (draws numFrames sf)

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

line :: Location -> Vector -> Double -> Color -> SuperForm
line loc dest thick col = 
  let minx = min (fst loc) (fst (loc + dest))
      miny = min (snd loc) (snd (loc + dest))
      maxx = max (fst loc) (fst (loc + dest))
      maxy = max (snd loc) (snd (loc + dest))
      box = (maxx, maxy) - (minx, miny)
  in primElev loc (minx, miny) box (Line dest thick col)

text :: Location -> BoundingBox -> String -> SuperForm
text loc box str = 
  primElev loc 
           (loc - (box / (2,2))) 
           box 
           (Text box str)

rekt :: Location -> BoundingBox -> Bool -> Color -> SuperForm
rekt loc box fill col = 
  primElev loc loc box (Rekt box fill col)

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
