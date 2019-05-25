module Shape where

type Vertex = (Float, Float)

data Shape = Rectangle Float Float
           | Ellipse Float Float
           | RtTriangle Float Float
           | Polygon [(Float, Float)]
  deriving Show
