{-# LANGUAGE MagicHash #-}
module Memo1 (memo1) where

import Data.IORef
import System.IO.Unsafe
-- import GHC.Prim
import GHC.Exts

unsafePtrEq :: a -> a -> Bool
unsafePtrEq x y = case reallyUnsafePtrEquality# x y of
      1# -> True
      _  -> False

memo1 :: (a->b) -> (a->b)
memo1 f = unsafePerformIO $ do
  cache <- newIORef []
  return $ \x -> unsafePerformIO $ do
              vals <- readIORef cache
              case x `inCache` vals of
                Nothing -> do let y = f x
                              writeIORef cache [(x,y)] -- ((x,y) :
--                                if null vals then [] else [head vals])
                              return y
                Just y  -> do return y

inCache :: a -> [(a,b)] -> Maybe b
_ `inCache` [] = Nothing
x `inCache` ((x',y'):xys) =
   if unsafePtrEq x x' then Just y' else x `inCache` xys
