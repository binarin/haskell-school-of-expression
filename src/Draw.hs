module Draw ( inchToPixel
            , pixelToInch
            , intToFloat
            , xWin
            , yWin
            , trans
            , shapeToGraphic
            , spaceClose
            , coloredShapes1
            ) where

import Shape
import Graphics.SOE
import Control.Monad (forM_)

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


sh1, sh2, sh3, sh4 :: Shape
sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5)
              ,(-1.5, 2.0)
              ,(-1.1, 0.2)
              ,(-1.7, -1.0)
              ,(-3.0, 0)
              ]

type ColoredShapes = [(Color, Shape)]

shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w ss = forM_ ss $ \(c, s) ->
  drawInWindow w $ withColor c $ shapeToGraphic s

coloredShapes1 :: IO ()
coloredShapes1 = do
  w <- openWindow "Drawing Shapes" (xWin, yWin)
  drawShapes w shs
  spaceClose w
