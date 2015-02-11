module Super.Canvas.JS (drawPlate) where

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

drawPrim :: (Double, Double) -> Context -> Primitive -> IO ()
drawPrim (x,y) c (Circle r f) =
  do 
     putStrLn ("Drawing a circle...")
     beginPath c 
     fillStyle 255 255 255 255 c
     strokeStyle 255 255 255 255 c
     arc x y r 0 (2 * pi) True c
     if f
        then fill c
        else stroke c
     
     return ()
drawPrim (x,y) c (Line a b l) =
  do 
     putStrLn ("Drawing a line...")
     
     beginPath c
     moveTo x y c
     lineTo a b c
     lineWidth 5 c
     strokeStyle 255 255 255 255 c
     stroke c
     
     return ()

drawShape :: Context -> Shape a -> IO ()
drawShape c (Shape _ prims coords) = 
  foldr (\p r -> drawPrim coords c p >> r) (return ()) prims

drawPlate :: Plate b -> IO ()
drawPlate ss = 
  do c <- cx
     save c
     clearRect 0 0 900 500 c
     
     foldr (\s r -> drawShape c s >> r) (return ()) ss
     restore c
     return ()
