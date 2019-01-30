{-|
Module      : Vec
Description : Small Vector module
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Small Vector module
-}

module Vec where

newtype Vec a = Vec { unVec :: (a, a, a) }
            deriving (Show)

instance Functor Vec where
  fmap f (Vec (a, b, c)) = Vec (f a, f b, f c)

findZ :: (Floating a) => a -> a -> Vec a
findZ x y =
  Vec (x, y, sqrt (1 - x^^2 - y^^2))

dot :: (Num a) => Vec a -> Vec a -> a
dot (Vec (x1, y1, z1)) (Vec (x2, y2, z2)) =
  x1 * x2 + y1 * y2 + z1 * z2

normalise :: (Floating a) => Int -> (Int, Int) -> Vec a
normalise size (y, x) =
  findZ (scale x) $ scale y
  where
    scale a = 2 * fromIntegral a / fromIntegral size - 1

reflect :: (Floating a) => Int -> Vec a -> (Int, Int) -> Vec a
reflect size v (y, x) =
  n
  where
    n = normalise size (y, x)
