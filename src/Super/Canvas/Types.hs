module Super.Canvas.Types ( Primitive (..)
                          , Shape (..)
                          , Layout (..)
                          , Scale (..)
                          , Plate (..) ) where

data Primitive = -- radius, fill
                 Circle Double Bool
                 -- xdest, ydest, lthick
               | Line Double Double Double

data Shape b = Shape { thing  :: b
                     , prims  :: [Primitive]
                     , coords :: (Double,Double) }

type Layout a b = (a -> Scale -> Plate b)

type Scale = (Double, Double)

type Plate b = [Shape b]
