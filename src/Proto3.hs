import Reactive.Banana
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random

import Super.Canvas
import Super.Trees

main = startCanvas "thecanvas" >>= treestuff

treestuff sc = 
  do t <- newAddHandler
     b <- newAddHandler
     attachButton "gen" (snd b)
     fd <- newAddHandler
     attachField "numlevels" (snd fd)
     g <- newStdGen
     let (rando,g') = random g
         (ref,nxt) = randomTrees 4 rando
     network <- compile (mkNet sc
                               ref 
                               (fst t)
                               (fst b)
                               (fst fd)
                               (snd t))
     actuate network
     -- write sc (format (prepSTree (fst thetrees)))
     (snd t) (ref, ([], nxt))
     putStrLn "Started?"
     return ()

mkNet sc ref t button field fire = 
  do eTrees <- fromAddHandler t
     eButton <- fromAddHandler button
     eField <- fromAddHandler field
     let eNums = fmap (tryread 4) eField
         bNum = stepper 4 eNums
         bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format fire) eTrees 
         eForms = eTreeForms
     reactimate (fmap (display sc) eForms)
     reactimate (fmap (newtrees fire) (bNum <@ eButton))

display :: SuperCanvas -> ([SuperForm], SuperForm) -> IO ()
display sc (as,s) = animate sc (100 * 1000) as >> write sc s

newtrees t n = do g <- newStdGen
                  let (rando,_) = random g
                      (ref,nxt) = randomTrees n rando
                  t (ref,([],nxt))

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n

format :: Handler ( BiTree (Bool, Color)
                  , ( [(Vector, SuperForm)]
                    , BiTree (Bool, Color)  ) )
       -> ( BiTree (Bool, Color)
          , ( [(Vector, SuperForm)]
            , BiTree (Bool, Color) ) )
       -> ([SuperForm], SuperForm)
format fire (ref, (ani, nxt)) = 
  let pRef = fit (50,50) (200,200) (prepSTree ref)
      pAni = fmap (fit (50,300) (200,180)) (travel 10 ani)
      pNxt = fit (50,300) 
                 (200, 180) 
                 (prepTree (\b -> fire (ref,b)) nxt)
      can = combine [ pRef, pNxt ]
      nextshow = 
        if ref == nxt
           then combine 
                  [ text (500,300) (200,150) "You Win!"
                  , can ]
           else can
      animates = fmap (\a -> combine [a,pRef]) pAni
  in (animates, nextshow)
