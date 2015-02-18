module Super.Canvas.Types ( Primitive (..)
                          , Shape (..)
                          , Layout (..)
                          , Scale (..)
                          , scale
                          , actio
                          , getActions
                          , QualAction (..)
                          , Plate (..) 
                          , Action (..) ) where

data Primitive = -- offset, radius, fill
                 Circle (Double,Double) Double Bool
                 -- offset-start, offset-dest, lthick
               | Line (Double,Double) (Double,Double) Double

data Shape b = Shape { thing   :: b
                     , bounds  :: (Double,Double)
                     , prims   :: [Primitive]
                     , coords  :: (Double,Double)
                     , actions :: [Action] }

data Action = OnClick (IO ()) | MouseOver (IO ())

actio (OnClick io) = io
actio (MouseOver io) = io

type QualAction = ((Double, Double), (Double, Double), Action)
type Layout a b = (a -> Scale -> Plate b)

type Scale = (Double, Double)

scale :: Scale -> Shape a -> Shape a
scale sc (Shape t b p c a) = 
  Shape t (mul sc b) p (mul sc c) a

mul (a,b) (c,d) = ( a * c , b * d )

type Plate b = [Shape b]



getActions :: Plate b -> [QualAction]
getActions = 
  foldr (\ (Shape _ b _ c as) mas -> 
           (fmap (qual c b) as) ++ mas) []

qual c b ac = (c,b,ac)
