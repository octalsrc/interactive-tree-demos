module Super.Canvas.Types ( Primitive (..)
                          , Shape (..)
                          , Layout (..)
                          , Scale (..)
                          , Color (..)
                          , scale
                          , advc
                          , actio
                          , getIO
                          , getActions
                          , QualAction (..)
                          , Plate (..) 
                          , Action (..) ) where

data Color = Red | Green | Blue | Yellow deriving (Show, Enum)

advc Yellow = Red
advc c = succ c

data Primitive = -- offset, radius, fill
                 Circle (Double,Double) Double Bool Color
                 -- offset-start, offset-dest, lthick
               | Line (Double,Double) (Double,Double) Double
                 -- offset, (width,height), text
               | Text (Double,Double) (Double,Double) String
                 -- offset-center, (width,height), fill
               | Rekt (Double,Double) (Double,Double) Color

data Shape b = Shape { thing   :: b
                     , bounds  :: (Double,Double)
                     , prims   :: [Primitive]
                     , coords  :: (Double,Double)
                     , actions :: [Action] }

data Action = OnClick (IO ()) | MouseOver (IO ())

actio (OnClick io) = io
actio (MouseOver io) = io

type QualAction = ((Double, Double), (Double, Double), Action)

getIO (_,_,ac) = actio ac

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
