{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

import Prelude (enumFrom, toEnum)

data Demo = DefaultDemo
          | RandomDemo
          | Sierpinski
          | FractalSnowflake
          | SomeColoredShapes
          | SimplePictureDemo
          | SimpleAnimation
          | ReactivityTest
  deriving (Read, Show, Enum, Eq)

allDemos :: [Demo]
allDemos = filter notNonDemo allValues
  where
    notDefault = (/= DefaultDemo)
    notRandom = (/= RandomDemo)
    notNonDemo x = notRandom x && notDefault x
    allValues = enumFrom (toEnum 0)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsDemo :: Demo
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
