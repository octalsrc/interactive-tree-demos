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
                          , Draw (..)
                          , getIO 
                          , QualAction (..) 
                          , Action (..) ) where

import System.Random
import qualified Data.List as L

data SuperForm = Node [SuperForm]
               | Leaf SCLeaf

data SCLeaf = Scale Factor SuperForm
            | Trans Vector SuperForm
            | Travel Vector SuperForm
            | Elem SCElem

data SCElem = Prim Primitive
            | Trigger BoundingBox [Action]
            | Bounds BoundingBox

type QElem = (Location, Factor, Vector, SCElem)

qList :: Int -> SuperForm -> [QElem]
qList numFrames = r initLoc initFactor initVector
  where initLoc = (0,0)
        initFactor = (1,1)
        initVector = (0,0)
        nf = numFrames

        r :: Location -> Factor -> Vector -> SuperForm -> [QElem]
        r pl pf pv (Node scs) = 
          foldr (\a -> (++) (r pl pf pv a)) [] scs
        r pl pf pv (Leaf (Trans l sc)) = r (pl + (l * pf)) pf pv sc
        r pl pf pv (Leaf (Scale f sc)) = r pl (pf * f) pv sc
        r pl pf pv (Leaf (Travel v sc)) = 
          r pl pf (pv + calcFrame nf (v * pf)) sc
        r pl pf pv (Leaf (Elem e)) = [(pl, pf, pv, e)]
        
calcFrame nf v = v / (fromIntegral nf, fromIntegral nf)

type Draw = (Location, Primitive)

draws :: Int -> SuperForm -> [[Draw]]
draws nf = L.transpose . fmap (mult nf) . fmap scalePrim' . prims nf

mult :: Int -> (Location, Vector, Primitive) -> [Draw]
mult nf qp = foldr (\n -> (:) (frameShift qp n)) [] [1..nf]

frameShift (l,v,p) frame = (l + toVector frame * v, p)

toVector :: Int -> Vector
toVector n = (fromIntegral n, fromIntegral n)

scalePrim' :: (Location, Factor, Vector, Primitive)
           -> (Location, Vector, Primitive)
scalePrim' (l,f,v,p) = (l, v, scalePrim f p)

prims :: Int ->  SuperForm -> [(Location, Factor, Vector, Primitive)]
prims nf = f . qList nf
  where f = foldr (\e as -> 
                     case e of
                       (l,f,v,(Prim p)) -> (l,f,v,p) : as
                       _ -> as) []

actions :: SuperForm -> [QualAction]
actions = concat . f . qList 1
  where f = foldr (\e as -> 
                     case e of
                       (l,f,v,(Trigger b acs)) -> 
                         q l (f * b) acs : as
                       _ -> as) []
        q l b = fmap (\a -> (l,b,a))

bounds sc = 
  let bs = foldr (\e as -> 
                 case e of
                   (l,f,v,(Bounds b)) -> 
                     (l, f * b) : as
                   _ -> as) []
      xl = fmap (fst . fst) (bs (qList 1 sc))
      yl = fmap (snd . fst) (bs (qList 1 sc))
      bbs = fmap (\(l,b) -> l + b) (bs (qList 1 sc))
      xs = fmap fst bbs
      ys = fmap snd bbs
  in ( (minimum xl, minimum yl)
     , (maximum xs - minimum xl, maximum ys - minimum yl))

data Color = Red | Green | Blue | Yellow | White | Gray | Black
             deriving (Show, Enum, Bounded, Eq)

instance Random Color where
  random g = 
    case randomR ( fromEnum (minBound::Color)
                 , fromEnum (maxBound::Color)) g of
      (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR ( fromEnum a
                                 , fromEnum b) g of
                      (r,g') -> (toEnum r, g')

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
               | Line Vector Double Color
                 -- (width,height), text
               | Text BoundingBox String
                 -- (width,height), fill, Color
               | Rekt BoundingBox Bool Color
               deriving (Show, Eq)

scalePrim :: Factor -> Primitive -> Primitive
scalePrim f (Circle r b c) = Circle (r * (fst f)) b c
scalePrim f (Line v t c) = Line (v * f) t c
scalePrim f (Text b s) = Text (b * f) s
scalePrim f (Rekt b fill c) = Rekt (b * f) fill c

data Action = OnClick (IO ()) -- maybe later.. | MouseOver (IO ())

actio (OnClick io) = io

type QualAction = (Location, BoundingBox, Action)

getIO (_,_,(OnClick io)) = io
