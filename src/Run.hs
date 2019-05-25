{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import RIO.List
import Graphics.SOE
import Data.List (cycle)
import System.Random (getStdRandom, randomR)

import Import
import Draw

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
runDemo DefaultDemo = coloredShapes1
runDemo RandomDemo = pickRandomDemo >>= runDemo
runDemo Sierpinski = serp
runDemo FractalSnowflake = runSnowflakeFractal
runDemo SomeColoredShapes = coloredShapes1

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


