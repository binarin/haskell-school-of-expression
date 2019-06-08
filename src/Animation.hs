module Animation where

import Shape
import Draw
import Picture
import SOE hiding (Region)
import qualified SOE as G (Region)

import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad (when)

type Time = Float
type Animation a = Time -> a

regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion

animate :: String -> Animation Graphic -> IO ()
animate title anim = do
  w <- openWindow title (xWin, yWin)
  t0 <- timeGetTime
  let loop = do
        t <- timeGetTime
        let ft = intToFloat (word32ToInt (t - t0)) / 1000
        setGraphic w (anim ft)
        spaceCloseEx w loop
  loop

picToGraphic :: Picture -> Graphic
-- picToGraphic (Region c r) = withColor c $ regionToGraphic r
-- picToGraphic (p1 `Over` p2) = picToGraphic p1 `overGraphic` picToGraphic p2
-- picToGraphic EmptyPic = emptyGraphic
picToGraphic p = foldr (\(c, r) g -> withColor c (regionToGraphic r) `overGraphic` g) emptyGraphic (picToList p)
