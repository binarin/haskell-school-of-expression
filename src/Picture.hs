module Picture
  ( Picture(..)
  , Color(..)
  , regionToGRegion
  , shapeToGRegion
  , drawRegionInWindow
  , drawPic
  -- , draw
  , spaceClose
  , module Region
  )  where

import Region
import Draw
import Graphics.SOE hiding (Region)
import qualified Graphics.SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
  deriving (Show)

drawRegionInWindow:: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r = drawInWindow w $ withColor c $ drawRegion $ regionToGRegion r

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r) = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = drawPic w p2 >> drawPic w p1
drawPic _ EmptyPic = pure ()


regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0, 0) (1, 1) r

primGReg :: Vector -> Vector -> Region -> Region -> (G.Region -> G.Region -> G.Region) -> G.Region
primGReg loc sca r1 r2 op = let gr1 = regToGReg loc sca r1
                                gr2 = regToGReg loc sca r2
                            in op gr1 gr2

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s) = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (dx, dy) r) = regToGReg loc (sx * dx, sy * dy) r
regToGReg (x, y) sca@(sx, sy) (Translate (dx, dy) r) = regToGReg (x + sx * dx, y + sy * dy) sca r
regToGReg loc sca (Complement r) = primGReg loc sca winRect r diffRegion
regToGReg loc sca (r1 `Union` r2) = orRegion (regToGReg loc sca r1) (regToGReg loc sca r2)
regToGReg loc sca (r1 `Intersect` r2) = andRegion (regToGReg loc sca r1) (regToGReg loc sca r2)
regToGReg _ _ Empty = createRectangle (0, 0) (0, 0)

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) (Rectangle s1 s2) = createRectangle (lx + sx * s1, ly + sy * s2)(lx + sx * s1, ly + sy * s2)
