module Super.Canvas ( circle
                    , line
                    , text
                    , rekt
                    , blank
                    , combine
                    , scale
                    , onClick
                    , addOnClick
                    , translate ) where

import Super.Canvas.Types
import Super.Canvas.JS

circle :: Location -> Double -> Bool -> Color -> SCanvas
circle loc rad fill col = primElev loc 
                                   (loc - (rad,rad)) 
                                   (rad * 2, rad * 2) 
                                   (Circle rad fill col)

line :: Location -> Vector -> Double -> SCanvas
line loc dest thick = 
  primElev loc loc (loc + dest) (Line dest thick)

text :: Location -> BoundingBox -> String -> SCanvas
text loc box str = 
  primElev loc 
           (loc - (box / (2,2))) 
           box 
           (Text box str)

rekt :: Location -> BoundingBox -> Color -> SCanvas
rekt loc box col = 
  primElev loc loc box (Rekt box col)

combine :: [SCanvas] -> SCanvas
combine = Node . fmap pullUp

pullUp :: SCanvas -> SCanvas
pullUp (Node ((Node n):[])) = Node n
pullUp n = n

scale :: Factor -> SCanvas -> SCanvas
scale f = Leaf . Scale f

translate :: Vector -> SCanvas -> SCanvas
translate v = Leaf . Trans v 

onClick :: [IO ()] -> Location -> BoundingBox -> SCanvas
onClick ios l b = 
  Leaf (Trans l (Leaf (Elem (Trigger b (fmap OnClick ios)))))

addOnClick :: [IO ()] -> SCanvas -> SCanvas
addOnClick ios sc = 
  let b = bounds sc
      a = onClick ios idLocation b
  in combine [sc, a]

blank :: SCanvas
blank = Node []

primElev :: Location -> Location -> BoundingBox -> Primitive 
         -> SCanvas
primElev loc boxloc box prim = 
  Node [ Leaf (Trans loc (Leaf (Elem (Prim prim))))
       , Leaf (Trans boxloc (Leaf (Elem (Bounds box))))]
