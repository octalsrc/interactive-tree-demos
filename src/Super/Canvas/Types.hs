{-# LANGUAGE FlexibleInstances #-}

module Super.Canvas.Types ( Primitive (..)
                          , Shape (..)
                          , Layout (..)
                          , Factor (..)
                          , Location (..)
                          , Vector (..)
                          , BoundingBox (..)
                          , Color (..)
                          , Traveller (..)
                          , Animate (..)
                          , scale
                          , nextColor
                          , actio
                          , getIO
                          , getActions
                          , translate
                          , QualAction (..)
                          , Plate (..) 
                          , Action (..) ) where

import System.Random

data Color = Red | Green | Blue | Yellow 
             deriving (Show, Enum, Bounded, Eq)

instance Random Color where
  random g = 
    case randomR ( fromEnum (minBound::Color)
                 , fromEnum (maxBound::Color)) g of
      (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR ( fromEnum a
                                 , fromEnum b) g of
                      (r,g') -> (toEnum r, g')

nextColor Yellow = Red
nextColor c = succ c

-- Be careful! this is not really a complete instance!
instance Num (Double, Double) where
  (+) (a,b) (c,d) = (a + c, b + d)
  (-) (a,b) (c,d) = (a - c, b - d)
  (*) (a,b) (c,d) = (a * c, b * d)
  abs t = (abs (fst t), abs (snd t))
  signum t = 0 -- signum ((fst t) * (snd t))
  fromInteger x = (0,0) -- (fromInteger x, fromInteger x)

-- Nor is this one! I'm just taking the operators and running!
instance Fractional (Double, Double) where
  (/) (a,b) (c,d) = (a / c, b / d)
  fromRational x = (0,0) -- (x, x)

type CanvasValue = (Double, Double)

type Location    = CanvasValue
type Factor      = CanvasValue
type BoundingBox = CanvasValue
type Vector      = CanvasValue

data Primitive = -- offset, radius, fill, Color
                 Circle (Double,Double) Double Bool Color
                 -- offset-start, offset-dest, lthick
               | Line (Double,Double) (Double,Double) Double
                 -- offset, (width,height), text
               | Text (Double,Double) (Double,Double) String
                 -- offset-center, (width,height), fill, Color
               | Rekt (Double,Double) (Double,Double) Color
               deriving (Show, Eq)

data Shape = Shape { bounds  :: BoundingBox 
                   , coords  :: Location
                   , prims   :: [Primitive]
                   , actions :: [Action]    }

instance Show Shape where
  show (Shape b c p _) = show (b, c, p)

data Action = OnClick (IO ()) | MouseOver (IO ())

actio (OnClick io) = io
actio (MouseOver io) = io

type QualAction = ((Double, Double), (Double, Double), Action)

getIO (_,_,ac) = actio ac

type Layout a = (a -> Plate)

scaleS :: Factor -> Shape -> Shape
scaleS sc (Shape b c p a) = 
  Shape (mul sc b) (mul sc c) p a

mul (a,b) (c,d) = ( a * c , b * d )

translateS :: Vector -> Shape -> Shape
translateS (vx,vy) (Shape b (x,y) p a) = 
  Shape b (x + vx, y + vy) p a

getActions :: Plate -> [QualAction]
getActions = 
  foldr (\ (Shape b c _ as) mas -> 
           (fmap (qual c b) as) ++ mas) []

qual c b ac = (c,b,ac)

type Plate = [Shape]

scale :: Factor -> Plate -> Plate
scale f = fmap (scaleS f)

translate :: Vector -> Plate -> Plate
translate v = fmap (translateS v)

class Animate a where
  animate :: Int   -- duration of animation (ms)
          -> Int   -- # frames of animation
          -> a     -- thing to animate
          -> IO () -- (blocking) animation action

type Traveller = (Plate, Location, Location)
