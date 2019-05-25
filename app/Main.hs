{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.List (intercalate)
import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_soe

import Prelude (enumFrom, toEnum)

allDemoNames :: String
allDemoNames = intercalate ", " $ show `fmap` enumFrom (toEnum 0 :: Demo)

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_soe.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option auto ( long "demo"
                      <> short 'd'
                      <> help ("which demo: " <> allDemoNames)
                      <> value DefaultDemo
                       )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
