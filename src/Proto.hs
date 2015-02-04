{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI #-}

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Default (def)

import GHCJS.Foreign
import GHCJS.Types
import JavaScript.JQuery
import JavaScript.Canvas
import Data.Text (pack, unpack)

main = 
  do putStrLn "Starting..."
     c <- newAddHandler
     m <- newAddHandler
     can <- select (pack "#thecanvas") >>= indexArray 0 . castRef >>= getContext
     attachHandlers (snd c) (snd m)
     network <- compile (mkNet can (fst c) (fst m))
     actuate network
     --insertS "shape 1" (30,50) 10
     --insertS "shape 2" (60,10) 5
     --insertS "shape 3" (30,80) 20
     return ()

mkNet can c m = 
  do eClicks <- fromAddHandler c
     eMoves <- fromAddHandler m

     reactimate (fmap (drawSmall can) eMoves)
     reactimate (fmap (drawBig can) eClicks)

attachHandlers c m = do can <- select (pack "#thecanvas")
                        let h ev = c =<< getMousePos ev
                        click h def can
                        let b ev = m =<< getMousePos ev
                        mousemove b def can
                        return ()

getMousePos ev = do x <- ffiGetMX ev
                    y <- ffiGetMY ev
                    return (x,y)

drawSmall c (x,y) = 
  do save c
     putStrLn $ "Drawing small at" ++ (show (x,y))
     beginPath c
     fillStyle 255 180 0 100 c
     arc (fromIntegral x) (fromIntegral y) 10 0 (2 * pi) True c
     fill c
     restore c
     return ()

drawBig c (x,y) = 
  do save c
     putStrLn $ "Drawing big at" ++ (show (x,y))
     beginPath c
     fillStyle 255 255 0 100 c
     arc (fromIntegral x) (fromIntegral y) 20 0 (2 * pi) True c
     fill c
     restore c
     return ()
     

foreign import javascript safe "$r = $1.clientX - document.getElementById(\"thecanvas\").getBoundingClientRect().left;"
   ffiGetMX :: JavaScript.JQuery.Event -> IO Int

foreign import javascript safe "$r = $1.clientY - document.getElementById(\"thecanvas\").getBoundingClientRect().top;"
   ffiGetMY :: JavaScript.JQuery.Event -> IO Int
