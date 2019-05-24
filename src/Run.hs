{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Graphics.SOE

run :: RIO App ()
run = do
  liftIO $ runGraphics $ do
    w <- openWindow "My First Graphics Program" (300, 300)
    drawInWindow w (text (100, 200) "Hello Graphics World")
    _ <- getKey w
    closeWindow w
  logInfo "We're inside the application!"
