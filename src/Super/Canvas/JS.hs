{-# LANGUAGE CPP, FlexibleInstances, 
    ForeignFunctionInterface, JavaScriptFFI,
    BangPatterns #-}

module Super.Canvas.JS ( getCanvas
                       , attachButton
                       , attachField
                       , clearcan
                       , writeToCanvas
                       , animateToCanvas
                       , attachClickHandler
                       , Context ) where

import Data.Default (def)
import Data.Text (pack, unpack)
import System.Random (newStdGen)

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

getCanvas name = selp ("#" ++ name)
                 >>= indexArray 0 . castRef 
                 >>= getContext

attachClickHandler name c = do can <- selp ("#" ++ name)
                               let h ev = c =<< getMousePos ev
                               click h def can 
                               return ()

attachButton name b = do but <- selp ("#" ++ name)
                         let h ev = b =<< newStdGen
                         click h def but
                         return ()
                         
attachField name f = do field <- selp ("#" ++ name)
                        let d = do val <- getVal field
                                   return (unpack val)
                            h ev = f =<< d
                        keyup h def field
                        return ()

getMousePos :: Event -> IO (Double, Double)
getMousePos ev = do x <- ffiGetMX ev
                    y <- ffiGetMY ev
                    return (fromIntegral x, fromIntegral y)

clearcan = clearRect 0 0 900 500

animateToCanvas :: Context -> Int -> [[Draw]] -> IO ()
animateToCanvas c i !pss = 
  sequence_ (fmap (writeWait c i) pss)

writeWait c i ps = writeToCanvas c ps >> threadDelay i

writeToCanvas :: Context -> [Draw] -> IO ()
writeToCanvas c !prims = 
  clearcan c >> sequence_ (fmap (writePrim c) prims)

writePrim :: Context -> Draw -> IO ()
writePrim c (l,p) = 
  let prim = p
      (x,y) = l
  in case prim of
       Circle r f col -> 
         do let (rc, gc, bc) = style col
            putStrLn ("Drawing a circle...")
            beginPath c 
            fillStyle rc gc bc 255 c
            strokeStyle 0 0 0 255 c
            arc x y r 0 (2 * pi) True c
            if f
               then fill c >> stroke c
               else stroke c 
            return ()
       Line (xd,yd) w ->
         do putStrLn ("Drawing a line...")
            beginPath c
            moveTo x y c
            lineTo (x + xd) (y + yd) c
            lineWidth w c
            strokeStyle 0 0 0 255 c
            stroke c
            return ()
       Text (w,h) s ->
         do fillStyle 0 0 0 255 c
            drawTextCenter (x,y) w h s c 
       Rekt (w,h) col ->
         do let (rc, gc, bc) = style col
            putStrLn ("Drawing a rekt...")
            fillStyle rc gc bc 255 c
            fillRect x y w h c
            return ()

style White = (255,255,255)
style Red = (255, 0, 0)
style Green = (0, 190, 0)
style Blue = (0, 0, 230)
style Yellow = (255, 170, 0)

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
