module Reactimate where

import Control.Monad (mapM_)
import FAL
import SOE hiding (Region, Event)
import qualified SOE as G (Region, Event)
import Draw (xWin, yWin, intToFloat)
import Concurrent

reactimate :: String -> Behavior a -> (a -> IO Graphic) -> IO ()
reactimate title franProg toGraphic = do
  w <- openWindow title (xWin, yWin)
  (user, addEvents) <- windowUser w
  addEvents
  let drawPic (Just p) = do
        g <- toGraphic p
        setGraphic w g
        addEvents
        -- getWindowTick w
        return ()
      drawPic Nothing = return ()
  mapM_ drawPic (runEvent (sample `snapshot_` franProg) user)

type User = ([Maybe UserAction], [Time])

runEvent :: Event a -> User -> [Maybe a]
runEvent (Event fe) u = fe u

sample :: Event ()
sample = Event $ \(us, _) -> map aux us
  where
    aux Nothing = Just ()
    aux (Just _) = Nothing

windowUser :: Window -> IO (([Maybe UserAction], [Time]), IO ())
windowUser w = do
  (evs, addEv) <- makeStream
  t0 <- timeGetTime
  let loop rt = do
        mev <- maybeGetWindowEvent w
        case mev of
          Nothing -> return ()
          Just e -> do addEv (Just e, rt)
                       loop rt
  let addEvents = do
        t <- timeGetTime
        let rt = w32ToTime (t - t0)
        loop rt
        addEv (Nothing, rt)
  return ((map fst evs, map snd evs), addEvents)


w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000

-- getWindowTick = undefined

makeStream :: IO ([a], a -> IO ())
makeStream = do
  c <- newChan
  as <- getChanContents c
  pure (as, writeChan c)
