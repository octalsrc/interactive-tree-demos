{-# LANGUAGE CPP, FlexibleInstances, 
    ForeignFunctionInterface, JavaScriptFFI #-}

module Super.Canvas.JS ( drawPlate
                       , drawPlateB
                       , printWin
                       , attachHandlers
                       , animate ) where

import Data.Default (def)
import Data.Text (pack, unpack)

import GHCJS.Foreign
import GHCJS.Types
import JavaScript.Canvas
import JavaScript.JQuery hiding (animate)

import Control.Concurrent
import Control.Monad

import Super.Canvas.Types

canvasName = "thecanvas"

selp = select . pack

sCanvas = selp ("#" ++ canvasName)

cx = sCanvas 
     >>= indexArray 0 . castRef 
     >>= getContext

printWin = do c <- cx
              drawTextCenter (600, 350) (350) (100) "You win!" c
attachHandlers c = do can <- sCanvas
                      let h ev = c =<< getMousePos ev
                      click h def can 
                      return ()

getMousePos :: Event -> IO (Double, Double)
getMousePos ev = do x <- ffiGetMX ev
                    y <- ffiGetMY ev
                    return (fromIntegral x, fromIntegral y)

drawPrim :: (Double, Double) -> Context -> Primitive -> IO ()
drawPrim (x,y) c (Circle (xo,yo) r f col) =
  do let (rc, gc, bc) = style col
     putStrLn ("Drawing a circle...")
     beginPath c 
     fillStyle rc gc bc 255 c
     strokeStyle 0 0 0 255 c
     arc (x + xo) (y + yo) r 0 (2 * pi) True c
     if f
        then fill c >> stroke c
        else stroke c 
     return ()
drawPrim (x,y) c (Line (xo,yo) (xd,yd) w) =
  do 
     putStrLn ("Drawing a line...")
     
     beginPath c
     moveTo (x + xo) (y + yo) c
     lineTo (x + xd) (y + yd) c
     lineWidth w c
     strokeStyle 0 0 0 255 c
     stroke c
     
     return ()
drawPrim (x,y) c (Rekt (xo,yo) (w,h) col) =
  do let (rc, gc, bc) = style col
     putStrLn ("Drawing a rekt...")
     fillStyle rc gc bc 255 c
     fillRect (x + xo) (y + yo) w h c
     return ()
drawPrim (x,y) c (Text (xo,yo) (w,h) s) =
  drawTextCenter ((x + xo),(y + yo)) w h s c

style Red = (255, 0, 0)
style Green = (0, 190, 0)
style Blue = (0, 0, 230)
style Yellow = (255, 170, 0)

drawShape :: Context -> Location -> Shape -> IO ()
drawShape c l (Shape _ coords ps _) = 
  foldr (\p r -> drawPrim (coords + l) c p >> r) (return ()) ps

clearcan = clearRect 0 0 900 500
clearcanB = clearRect 0 250 900 500

drawPlate :: Location -> Plate -> IO ()
drawPlate l ss = 
  do c <- cx
     save c
     clearRect 0 0 900 500 c
     
     foldr (\s r -> drawShape c l s >> r) (return ()) ss
     restore c
     return ()

drawPlateB :: Location -> Plate -> IO ()
drawPlateB l ss = 
  do c <- cx
     save c
     clearcanB c

     foldr (\s r -> drawShape c l s >> r) (return ()) ss
     restore c
     return ()

drawPlates :: [(Location, Plate)] -> IO ()
drawPlates ps = do c <- cx 
                   save c
                   clearcanB c
                   foldr (\(l,p) r -> drawPlate' c l p >> r)
                         (return ()) ps
                   restore c
                   return ()

drawPlate' :: Context -> Location -> Plate -> IO ()
drawPlate' c l ps = foldr (\s r -> drawShape c l s >> r) (return ()) ps

type Coord = (Double, Double)

drawTextCenter :: Coord   -- location at which to center the text
               -> Double  -- maximum width of the text
               -> Double  -- maximum height of the text
               -> String  -- the text to be drawn
               -> Context -- the canvas context
               -> IO ()
drawTextCenter (x,y) maxW maxH s c =
  do (a,b) <- setFont maxH maxW s c
     fillText (pack s) (x - (a / 2)) (y + (b / 2)) c

-- same as drawTextCenter, but floors the text at the coordinates
drawTextFloor :: Coord -> Double -> Double -> String -> Context -> IO ()
drawTextFloor (x,y) maxW maxH s c =
  do (a,_) <- setFont maxH maxW s c
     fillText (pack s) (x - (a / 2)) y c

setFont :: Double -> Double -> String -> Context -> IO (Double, Double)
setFont maxHeight maxWidth s c = try maxWidth maxHeight s c

fontPrecision = 6 -- size of steps taken when choosing a font
panicSize = 1 -- size to choose if algorithm bottoms out
try d f s c = do font (pack ((show ((floor f)::Int)) ++ "pt Calibri")) c
                 x <- measureText (pack s) c
                 if x > d
                    then if x > 0
                            then try d (f - fontPrecision) s c 
                            else print ("hit bottom..") 
                                 >> return (panicSize,f)
                    else print (show (floor f)) >> return (x,f)

{- Traveller is going to need to change to a newtype so that
   other Animate instances can be written for other Animates
   that may technically have the same type TODO-}
instance Animate [Traveller] where
  animate d s ts = let steps = fmap (calcStep s) ts
                       qts = zip ts steps
                       time = d `div` s
                   in (repeatM s (travel time) $! qts) >> return ()

travel :: Int -> [(Traveller, Vector)] -> IO [(Traveller, Vector)]
travel time qts = 
  let newqts = fmap (\((p,s,d),v) -> ((p,(s + v),d), v)) qts
  in drawPlates (fmap (\((p,s,d),v) -> (s,p)) newqts)
     -- foldr (\((p,s,_),_) io -> io >> drawPlate s p) (return ()) qts
     >> threadDelay (time * 1000)
     >> return (newqts)

-- I imagine this is already a function, but I couldn't find it...
repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM n f v = if n > 0
                   then f v >>= (repeatM (n - 1) f)
                   else return v

calcStep :: Int -> Traveller -> Vector
calcStep s (_, start, end) = (end - start) / ( fromIntegral s
                                             , fromIntegral s)

foreign import javascript safe "$r = $1.clientX - document.getElementById(\"thecanvas\").getBoundingClientRect().left;"
   ffiGetMX :: JavaScript.JQuery.Event -> IO Int

foreign import javascript safe "$r = $1.clientY - document.getElementById(\"thecanvas\").getBoundingClientRect().top;"
   ffiGetMY :: JavaScript.JQuery.Event -> IO Int
