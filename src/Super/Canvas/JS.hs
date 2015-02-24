{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI #-}

module Super.Canvas.JS ( drawPlate
                       , getMousePos ) where

import Data.Default (def)
import Data.Text (pack, unpack)

import GHCJS.Foreign
import GHCJS.Types
import JavaScript.Canvas
import JavaScript.JQuery

import Super.Canvas.Types

cx = select (pack "#thecanvas") 
     >>= indexArray 0 . castRef 
     >>= getContext

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
     strokeStyle 255 255 255 255 c
     arc (x + xo) (y + yo) r 0 (2 * pi) True c
     if f
        then fill c
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

drawShape :: Context -> Shape -> IO ()
drawShape c (Shape _ coords ps _) = 
  foldr (\p r -> drawPrim coords c p >> r) (return ()) ps

drawPlate :: Plate -> IO ()
drawPlate ss = 
  do c <- cx
     save c
     clearRect 0 0 900 500 c
     
     foldr (\s r -> drawShape c s >> r) (return ()) ss
     restore c
     return ()

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



foreign import javascript safe "$r = $1.clientX - document.getElementById(\"thecanvas\").getBoundingClientRect().left;"
   ffiGetMX :: JavaScript.JQuery.Event -> IO Int

foreign import javascript safe "$r = $1.clientY - document.getElementById(\"thecanvas\").getBoundingClientRect().top;"
   ffiGetMY :: JavaScript.JQuery.Event -> IO Int
