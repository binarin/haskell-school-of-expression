module Draw ( inchToPixel
            , pixelToInch
            , intToFloat
            , xWin
            , yWin
            , trans
            , shapeToGraphic
            , spaceClose
            ) where

import Shape
import Graphics.SOE

inchToPixel :: Float -> Int
inchToPixel x = round $ x * 100

pixelToInch :: Int -> Float
pixelToInch n = intToFloat n / 100

intToFloat :: Int -> Float
intToFloat = fromIntegral

xWin, yWin :: Int
xWin = 1200
yWin = 1000

trans :: Vertex -> Point
trans (x, y) = (xWin2 + inchToPixel x, yWin2 - inchToPixel y)

xWin2, yWin2 :: Int
xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

transList :: [Vertex] -> [Point]
transList = fmap trans


spaceClose :: Window -> IO ()
spaceClose w = do
  k <- getKey w
  if k == ' ' then closeWindow w
              else spaceClose w

shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Rectangle s1 s2) = polygon $ transList [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)]
  where s12 = s1 / 2
        s22 = s2 / 2
shapeToGraphic (Ellipse r1 r2) =
  ellipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGraphic (RtTriangle s1 s2) =
  polygon $ transList [(0, 0), (s1, 0), (0, s2)]
shapeToGraphic (Polygon lst) = polygon $ transList lst
