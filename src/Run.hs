{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import RIO.List
import SOE
import Data.List (cycle)
import System.Random (getStdRandom, randomR)

import Import
import Draw
import Picture

pickRandomDemo :: IO Demo
pickRandomDemo = do
  idx <- getStdRandom (randomR (0, length allDemos - 1))
  case headMaybe (drop idx allDemos) of
    Nothing -> pure DefaultDemo
    Just demo -> pure demo

runnableDemo :: Demo -> RIO App Demo
runnableDemo DefaultDemo = pure SomeColoredShapes
runnableDemo RandomDemo = liftIO $ pickRandomDemo
runnableDemo d = pure d

runDemo :: Demo -> IO ()
runDemo DefaultDemo = simplePictureDemo
runDemo RandomDemo = pickRandomDemo >>= runDemo
runDemo Sierpinski = serp
runDemo FractalSnowflake = runSnowflakeFractal
runDemo SomeColoredShapes = coloredShapes1
runDemo SimplePictureDemo = simplePictureDemo

run :: RIO App ()
run = do
  demo <- optionsDemo <$> asks appOptions
  demo' <- runnableDemo demo
  logInfo $ "Running demo " <> displayShow demo'
  liftIO $ runGraphics $ runDemo demo'


runSnowflakeFractal :: IO ()
runSnowflakeFractal = do
  w <- openWindow "Snowflake" (1000, 1000)
  snowflakeFractal w (cycle [Yellow, Blue, Green, Cyan, Red]) 500 500 330
  spaceClose w

snowflakeFractal :: Window -> [Color] -> Int -> Int -> Int -> IO ()
snowflakeFractal w colors xc yc size
 | size < minSize = pure ()
 | otherwise = let
      sd :: Double = fromIntegral size
      angles1 = [3*pi/2-pi/3, 3*pi/2+pi/3,pi/2]
      angles2 = [pi/2-pi/3, pi/2+pi/3,3*pi/2]
      px a = xc + round (cos a * sd)
      py a = yc + round (sin a * sd)
      (c, cs) = case colors of
        [c'] -> (c', [c'])
        (c':cs') -> (c', cs')
        _ -> (White, [White])
      in do
        drawInWindow w $ withColor c $ do
          polygon $ [ (px a, py a) | a <- angles1 ]
          polygon $ [ (px a, py a) | a <- angles2 ]
        forM_ (angles1++angles2) $ \a -> do
          snowflakeFractal w cs (px a) (py a) (size `div` 3)


serp :: IO ()
serp = do
    w <- openWindow "My First Graphics Program" (400, 400)
    sierpinskiTri w 50 300 256
    spaceClose w

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size =
  drawInWindow w $
  withColor Blue $
  polygon [(x, y), (x + size, y), (x, y - size)]

minSize :: Int
minSize = 8

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size =
  if size <= minSize
  then fillTri w x y size
  else let size2 = size `div` 2
       in do sierpinskiTri w x y size2
             sierpinskiTri w x (y - size2) size2
             sierpinskiTri w (x + size2) y size2

simplePictureDemo :: IO ()
simplePictureDemo = do
  w <- openWindow "it" (xWin, yWin)
  clickableRegionsLoop w (picToList tpp2)
  where
    r1 = Shape (Rectangle 3 2)
    r2 = Shape (Ellipse 1 1.5)
    r3 = Shape (RtTriangle 3 2)
    r4 = Shape (Polygon ([(-2.5,  2.5), (-3.0, 0), (-1.7, -1.0), (-1.1, 0.2), (-1.5, 2.0)]))
    reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
    pic1 = Region Blue reg1
    reg2 = let circle = Shape (Ellipse 0.5 0.5)
               square = Shape (Rectangle 1 1)
           in Scale (2, 2) circle
              `Union` Translate (1,0) square
              `Union` Translate (-1, 0) square
    pic2 = Region Yellow $ Translate (0, -1) reg2
    pic3 = pic2 `Over` pic1

    tp1 = Shape $ Polygon $ [(1, 1), (1, 2), (2, 1)]
    tp2 = Shape $ Polygon $ [(2, 2), (2, 3), (3, 2)]
    tpp1 = Region Blue r3
    tpp2 = Region Red r4
    tp = tpp1 `Over` tpp2
    -- drawInWindow w $ withColor Blue $
  -- drawInWindow w $ withColor Red $ polygon [(200, 200), (200, 300), (300, 200)]
