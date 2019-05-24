{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Graphics.SOE
import Data.List (cycle)

run :: RIO App ()
run = do
  liftIO $ runGraphics $ runSnowflakeFractal
  logInfo "We're inside the application!"


runSnowflakeFractal :: IO ()
runSnowflakeFractal = do
  w <- openWindow "Snowflake" (1000, 1000)
  -- fillStar w 450 450 (769 `div` 2)
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

spaceClose :: Window -> IO ()
spaceClose w = do
  k <- getKey w
  if k == ' ' then closeWindow w
              else spaceClose w

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


fillStar :: Window -> Int -> Int -> Int -> IO ()
fillStar w xc yc size = drawInWindow w $ do
  polygon [ (xc, yc - size)
          , (xc + cosine, yc + size2)
          , (xc - cosine, yc + size2)
          ]
  polygon [ (xc, yc + size)
          , (xc + cosine, yc - size2)
          , (xc - cosine, yc - size2)
          ]
  where
    cosine = floor (fromIntegral size * cos (pi @Double / 6.0))
    size2 = size `div` 2
