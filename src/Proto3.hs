import Reactive.Banana
import Reactive.Banana.Frameworks

import Text.Read (readMaybe)
import System.Random

import Super.Canvas
import Super.Trees

main = startCanvas "thecanvas" >>= treestuff


testshapes = combine [ circle (200, 150) 10 True Blue 
                     , rekt (100, 300) (10,60) Green  ]

testshapes2 = translate (450,50) ((prepSTree . fst) 
                                    (randomTrees 3 89))
                                    
treestuff sc = 
  do t <- newAddHandler
     b <- newAddHandler
     attachButton "gen" (snd b)
     fd <- newAddHandler
     attachField "numlevels" (snd fd)
     g <- newStdGen
     let (rando,g') = random g
         thetrees = randomTrees 4 rando
     network <- compile (mkNet sc
                               (fst thetrees) 
                               (fst t)
                               (fst b)
                               (fst fd)
                               (snd t))
     actuate network
     -- write sc (format (prepSTree (fst thetrees)))
     (snd t) thetrees
     putStrLn "Started?"
     return ()

mkNet sc ref t button field fire = 
  do eTrees <- fromAddHandler t
     eButton <- fromAddHandler button
     eField <- fromAddHandler field
     let eNums = fmap (tryread 4) eField
         bNum = stepper 4 eNums
         bRef = stepper ref (fmap fst eTrees)
         eTreeForms = fmap (format3 fire) eTrees 
         eForms = eTreeForms
     reactimate (fmap (write sc) eForms)
     reactimate (fmap (newtrees fire) (bNum <@ eButton))

newtrees t n = do g <- newStdGen
                  let (rando,_) = random g
                      tr = randomTrees n rando
                  t tr

format :: SuperForm -> SuperForm
format = translate (50,50)

tryread n s = case readMaybe s of
                Just i -> i
                _ -> n


format3 fire trees = 
  let can = combine [ fit (50,50) (200,200) (prepSTree (fst trees))
                    , fit (50,300) 
                          (200,180) 
                          (prepTree (\b -> fire (fst trees,b)) (snd trees))]
      (ref,test) = trees
  in if ref == test
        then combine [ text (500,300) (200,150) "You Win!"
                     , can ]
        else can
