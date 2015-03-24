{-# LANGUAGE FlexibleInstances #-}

module Super.Canvas.Types ( Primitive (..)
                          , SCLeaf (..)
                          , SCElem (..)
                          , QElem (..)
                          , SuperForm (..) 
                          , Factor (..)
                          , idFactor
                          , actions
                          , bounds
                          , draws
                          , idLocation
                          , Location (..)
                          , Vector (..)
                          , idVector
                          , BoundingBox (..)
                          , Color (..) 
                          , nextColor
                          , prims
                          , scalePrim
                          , Draw (..)
                          , actio
                          , getIO 
                          , QualAction (..) 
                          , Action (..) ) where

import System.Random

data SuperForm = Node [SuperForm]
             | Leaf SCLeaf

data SCLeaf = Scale Factor SuperForm
            | Trans Vector SuperForm
            | Elem SCElem

data SCElem = Prim Primitive
            | Trigger BoundingBox [Action]
            | Bounds BoundingBox

type QElem = (Location, Factor, SCElem)

qList :: SuperForm -> [QElem]
qList = r initLoc initFactor
  where initLoc = (0,0)
        initFactor = (1,1)
        r :: Location -> Factor -> SuperForm -> [QElem]
        r pl pf (Node scs) = foldr (\a -> (++) (r pl pf a)) [] scs
        r pl pf (Leaf (Trans l sc)) = r (pl + l) pf sc
        r pl pf (Leaf (Scale f sc)) = r pl (pf * f) sc
        r pl pf (Leaf (Elem e)) = [(pl * pf, pf, e)]

type Draw = (Location, Primitive)

draws :: SuperForm -> [Draw]
draws = fmap (\(l,f,p) -> (l, scalePrim f p)) . prims

prims :: SuperForm -> [(Location, Factor, Primitive)]
prims = f . qList
  where f = foldr (\e as -> 
                     case e of
                       (l,f,(Prim p)) -> (l,f,p) : as
                       _ -> as) []

actions :: SuperForm -> [QualAction]
actions = concat . f . qList
  where f = foldr (\e as -> 
                     case e of
                       (l,f,(Trigger b acs)) -> 
                         q l (f * b) acs : as
                       _ -> as) []
        q l b = fmap (\a -> (l,b,a))

bounds sc = 
  let bs = foldr (\e as -> 
                 case e of
                   (l,f,(Bounds b)) -> 
                     (l, f * b) : as
                   _ -> as) []
      xl = fmap (fst . fst) (bs (qList sc))
      yl = fmap (snd . fst) (bs (qList sc))
      bbs = fmap (\(l,b) -> l + b) (bs (qList sc))
      xs = fmap fst bbs
      ys = fmap snd bbs
  in ( (minimum xl, minimum yl)
     , (maximum xs - minimum xl, maximum ys - minimum yl))

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

idVector = (0,0) :: Vector
idFactor = (1,1) :: Factor
idLocation = (0,0) :: Location

data Primitive = -- radius, fill, Color
                 Circle Double Bool Color
                 -- offset-dest, lthick
               | Line Vector Double
                 -- (width,height), text
               | Text BoundingBox String
                 -- (width,height), fill, Color
               | Rekt BoundingBox Color
               deriving (Show, Eq)

scalePrim :: Factor -> Primitive -> Primitive
scalePrim f (Circle r b c) = Circle (r * (fst f)) b c
scalePrim f (Line v t) = Line (v * f) t
scalePrim f (Text b s) = Text (b * f) s
scalePrim f (Rekt b c) = Rekt (b * f) c

{-
        
data Shape = Shape { bounds  :: BoundingBox 
                   , coords  :: Location
                   , prims   :: [Primitive]
                   , actions :: [Action]    }

instance Show Shape where
  show (Shape b c p _) = show (b, c, p)
        -}
data Action = OnClick (IO ()) -- maybe later.. | MouseOver (IO ())

actio (OnClick io) = io
--actio (MouseOver io) = io

type QualAction = (Location, BoundingBox, Action)

getIO (_,_,ac) = actio ac

{-
        
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
        -}
