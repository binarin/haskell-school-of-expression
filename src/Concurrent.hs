module Concurrent 
  (Chan, isEmptyChan, newChan, readChan, writeChan,
   M.MVar, readMVar, newMVar, modifyMVar, modifyMVar_, getChanContents) where

import System.IO.Unsafe ( unsafeInterleaveIO )
import Control.Concurrent.MVar
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM as S
import qualified Control.Concurrent.STM.TChan as C

type Chan = S.TChan

isEmptyChan :: Chan a -> IO Bool
isEmptyChan = S.atomically . C.isEmptyTChan

newChan :: IO (Chan a)
newChan = C.newTChanIO

readChan :: Chan a -> IO a
readChan = S.atomically . C.readTChan

writeChan :: Chan a -> a -> IO ()
writeChan c = S.atomically . C.writeTChan c

getChanContents :: Chan a -> IO [a]
getChanContents chan = unsafeInterleaveIO $ do
  x <- readChan chan
  xs <- getChanContents chan
  return (x:xs)
