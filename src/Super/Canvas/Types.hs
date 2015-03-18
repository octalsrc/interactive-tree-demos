{-# LANGUAGE FlexibleInstances #-}

module Super.Canvas.Types ( Primitive (..)
                          , SCanvas (..) 
                          , Factor (..)
                          , Location (..)
                          , Vector (..)
                          , BoundingBox (..)
                          , Color (..) 
                          , nextColor
                          , scalePrim
                          , actio
                          , getIO 
                          , QualAction (..) 
                          , Action (..) ) where

import System.Random

data SCanvas = Node Location Factor [SCanvas]
             | Prim Primitive
             | Trigger BoundingBox [Action]
             | Bounds BoundingBox


-- all the recursive walking in these needs to be broken down,
-- probably into an instance of Traversable?

prims :: SCanvas -> [(Location, Factor, Primitive)]
prims = r initLoc initFactor
  where initLoc = (0,0)
        initFactor = (1,1)
        r :: Location -> Factor -> SCanvas
          -> [(Location, Factor, Primitive)]
        r pl pf (Node l f scs) = 
          foldr (\sc scs -> (r (pl + (l * pf)) (pf * f) sc) ++ scs) [] scs
        r pl pf (Prim prim) = [(pl, pf, prim)]
        r _ _ _ = []

actions :: SCanvas -> [(Location, BoundingBox, Action)]
actions = r initLoc initFactor
  where initLoc = (0,0)
        initFactor = (1,1)
        r :: Location -> Factor -> SCanvas
          -> [(Location, BoundingBox, Action)]
        r pl pf (Node l f scs) =
          foldr (\sc scs -> (r (pl + (l * pf)) (pf * f) sc) ++ scs) [] scs
        r pl pf (Trigger bb acs) = 
          fmap (\a -> (pl, (bb * pf), a)) acs
        r _ _ _ = []

bounds sc = let xs = fmap fst (bounds' sc)
                ys = fmap snd (bounds' sc)
            in (maximum xs, maximum ys)

bounds' :: SCanvas -> [BoundingBox]
bounds' = r initLoc initFactor
  where initLoc = (0,0)
        initFactor = (1,1)
        r :: Location -> Factor -> SCanvas
          -> [BoundingBox]
        r pl pf (Node l f scs) =
          foldr (\sc scs -> (r (pl + (l * pf)) (pf * f) sc) ++ scs) [] scs
        r pl pf (Bounds bb) =
          [pl + (pf * bb)]

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
