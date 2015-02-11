{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI #-}

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Default (def)

import GHCJS.Foreign
import GHCJS.Types
import JavaScript.JQuery
import JavaScript.Canvas
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
     can <- select (pack "#thecanvas") >>= indexArray 0 . castRef >>= getContext
     attachHandlers (snd c) (snd m)
     mkInput (snd i)
     network <- compile (mkNet can (fst c) (fst m) (fst i))
     actuate network
     --insertS "shape 1" (30,50) 10
     --insertS "shape 2" (60,10) 5
     --insertS "shape 3" (30,80) 20
     return ()

mkNet can c m i = 
  do eClicks <- fromAddHandler c
     eMoves <- fromAddHandler m
     eKeys <- fromAddHandler i
     
     reactimate (fmap (drawSmall can) eMoves)
     reactimate (fmap (drawBig can) eClicks)
     reactimate (fmap drawThing eKeys)

sHeader = select (pack ("#num"))
sInput = select (pack ("#in"))

mkInput fire = do inp <- select (pack "<input id=\"in\" />")
                  par <- select (pack "#num")
                  appendJQuery inp par
                  let h _ = sInput >>= fmap unpack . getVal >>= fire
                  keyup h def inp 

attachHandlers c m = do can <- select (pack "#thecanvas")
                        let h ev = c =<< getMousePos ev
                        click h def can
                        let b ev = m =<< getMousePos ev
                        mousemove b def can
                        return ()

getMousePos ev = do x <- ffiGetMX ev
                    y <- ffiGetMY ev
                    return (x,y)

drawThing s = case readMaybe s of
                Just x -> let t = BiTree () 
                                         (sampleTree $ x - 1)
                                         (sampleTree $ x - 1)
                          in if x > 0
                                then drawPlate (drawTree (450, 30)
                                                         t
                                                         (20,20)) 
                                else drawPlate []
                                     >> putStrLn ("tree of zero")
                _ -> drawPlate [] >> putStrLn ("invalid input..." ++ s)

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
