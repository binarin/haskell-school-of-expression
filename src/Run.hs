{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import RIO.List
import SOE hiding (Region)
-- import qualified SOE as G (Region)
import Data.List (cycle)
import System.Random (getStdRandom, randomR)

import Import
import Draw
import Picture
import Animation
import Reactimate
import FAL

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
runDemo SimpleAnimation = simpleAnimationDemo
runDemo ReactivityTest = simpleReactiveDemo

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
        drawInWindow w $ withColor c $ overGraphics
          [ polygon $ [ (px a, py a) | a <- angles1 ]
          , polygon $ [ (px a, py a) | a <- angles2 ]
          ]
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
  clickableRegionsLoop w (picToList pic3)
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

simpleAnimationDemo :: IO ()
simpleAnimationDemo = do
  animate "Animated Shape" (picToGraphic . planets)

rubberBall :: Animation Shape
rubberBall t = Ellipse (sin t) (cos t)

revolvingBall :: Animation Region
revolvingBall t = let ball = Shape (Ellipse 0.2 0.2)
                  in Translate (sin t, cos t) ball

planets :: Animation Picture
planets t = let p1 = Region Red (Shape (rubberBall t))
                p2 = Region Yellow (revolvingBall t)
            in p1 `Over` p2

simpleReactiveDemo :: IO ()
simpleReactiveDemo = reactimate "test" ball3 (pure . picToGraphic)

ball3 = paint color4 circ3
circ3 = translate mouse (ell (pure 0.2) (pure 0.2))

paint :: Behavior Color -> Behavior Region -> Behavior Picture
paint c r = Region <$> c <*> r

color4 = white `switch` (key `snapshot` color4 =>> \(c, old) ->
                            case c of 'R' -> red
                                      'B' -> blue
                                      'Y' -> yellow
                                      _ -> pure old)

ell :: Behavior Float -> Behavior Float -> Behavior Region
ell r1 r2 = Shape <$> (Ellipse <$> r1 <*> r2)
