{-# LANGUAGE LambdaCase #-}
module Picture
  ( Picture(..)
  , Color(..)
  , regionToGRegion
  , shapeToGRegion
  , drawRegionInWindow
  , drawPic
  , draw
  , spaceClose
  , module Region
  , clickableRegionsLoop
  , picToList
  )  where


import Control.Monad (forM_)
import Region
import Draw hiding (trans)
import SOE hiding (Region)
import qualified SOE as G (Region)

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
shapeToGRegion (lx, ly) (sx, sy) = \case
  Rectangle s1 s2 -> createRectangle (trans (-s1/2, -s2/2)) (trans (s1/2, s2/2))
  Ellipse r1 r2 -> createEllipse (trans (-r1, -r2)) (trans (r1, r2))
  RtTriangle s1 s2 -> createPolygon [trans (0, 0), trans (s1, 0), trans (0, s2)]
  Polygon points -> createPolygon $ trans `fmap` points
  where
    trans :: Vertex -> Point
    trans (x, y) = (xWin2 + inchToPixel (lx + x * sx)
                   ,yWin2 - inchToPixel (ly + y * sy)
                   )
    xWin2 = xWin `div` 2
    yWin2 = yWin `div` 2

draw :: String -> Picture -> IO ()
draw s p = do
  w <- openWindow s (xWin, yWin)
  drawPic w p
  spaceClose w

picToList :: Picture -> [(Color, Region)]
picToList (Region c r) = [(c, r)]
picToList EmptyPic = []
picToList (r1 `Over` r2) = picToList r1 ++ picToList r2

handleRegionListClick
  :: [(Color, Region)]
  -> Coordinate
  -> ( Maybe (Color, Region)
     , [(Color, Region)]
     )
handleRegionListClick [] _ = (Nothing, [])
handleRegionListClick ((c, r):rs) coord =
  if r `containsR` coord
  then (Just (c,r), rs)
  else let (hit, rs') = handleRegionListClick rs coord
       in (hit, (c,r):rs')

clickableRegionsLoop :: Window -> [(Color, Region)] -> IO ()
clickableRegionsLoop w regs = do
  clearWindow w
  forM_ (reverse regs) $ \(c, r) -> do
    drawRegionInWindow w c r
  (x, y) <- getLBP w
  case handleRegionListClick regs (pixelToInch $ x - xWin2
                                  ,pixelToInch $ yWin2 - y
                                  ) of
    (Nothing, _) -> closeWindow w
    (Just hit, newRegs) -> clickableRegionsLoop w (hit:newRegs)
  where
    xWin2 = xWin `div` 2
    yWin2 = yWin `div` 2
