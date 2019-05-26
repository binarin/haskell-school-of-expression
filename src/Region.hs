module Region
  ( module Shape
  , Region(..)
  , Coordinate
  , Vector
  , containsS
  , containsR
  ) where

import Shape

infixr 5 `Union`
infixr 6 `Intersect`

type Vector = (Float, Float)
type Coordinate = (Float, Float)

data Region = Shape Shape
            | Translate Vector Region
            | Scale Vector Region
            | Complement Region
            | Region `Union` Region
            | Region `Intersect` Region
            | Empty
  deriving (Show)


containsS :: Shape -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x, y)
  = let t1 = s1 / 2
        t2 = s2 / 2
    in -t1 <= x && x <= t1 && -t2 <= y && y <= t2
(Ellipse r1 r2) `containsS` (x, y)
  = (x/r1)^(2::Int)+(y/r2)^(2::Int) <= 1
(Polygon pts) `containsS` p
  = let lefOfList = map isLeftOfp (zip pts (tail pts ++ [head pts]))
        isLeftOfp p' = isLeftOf p p'
    in and lefOfList
RtTriangle s1 s2 `containsS` p
  = let vs = if signum s1 == signum s2
             then [(0, 0), (s1, 0), (0, s2)]
             else [(0, 0), (0, s2), (s1, 0)]
    in Polygon vs `containsS` p

type Ray = (Coordinate, Coordinate)

isLeftOf :: Coordinate -> Ray -> Bool
(px, py) `isLeftOf` ((ax, ay), (bx, by))
  = let (s, t) = (px - ax, py - ay)
        (u, v) = (px - bx, py - by)
    in s * v >= t * u

containsR :: Region -> Coordinate -> Bool
Shape s `containsR` p = s `containsS` p
Translate (u, v) r `containsR` (x, y) = r `containsR` (x - u, y -v)
Scale (u, v) r `containsR` (x, y) = r `containsR` (x/u, y/v)
Complement r `containsR` p = not $ r `containsR` p
(r1 `Union` r2) `containsR` p = r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p = r1 `containsR` p && r2 `containsR` p
Empty `containsR` _ = False
