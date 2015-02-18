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

style Red = (255, 0, 0)
style Green = (0, 190, 0)
style Blue = (0, 0, 230)
style Yellow = (255, 170, 0)

drawShape :: Context -> Shape a -> IO ()
drawShape c (Shape _ _ prims coords _) = 
  foldr (\p r -> drawPrim coords c p >> r) (return ()) prims

drawPlate :: Plate b -> IO ()
drawPlate ss = 
  do c <- cx
     save c
     clearRect 0 0 900 500 c
     
     foldr (\s r -> drawShape c s >> r) (return ()) ss
     restore c
     return ()

foreign import javascript safe "$r = $1.clientX - document.getElementById(\"thecanvas\").getBoundingClientRect().left;"
   ffiGetMX :: JavaScript.JQuery.Event -> IO Int

foreign import javascript safe "$r = $1.clientY - document.getElementById(\"thecanvas\").getBoundingClientRect().top;"
   ffiGetMY :: JavaScript.JQuery.Event -> IO Int
