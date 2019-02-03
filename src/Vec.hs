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

instance (Num a) => Num (Vec a) where
  (Vec (x1, y1, z1)) + (Vec (x2, y2, z2)) = Vec (x1 + x2, y1 + y2, z1 + z2)
  (Vec (x1, y1, z1)) - (Vec (x2, y2, z2)) = Vec (x1 - x2, y1 - y2, z1 - z2)
  (Vec (x1, y1, z1)) * (Vec (x2, y2, z2)) = Vec (x1 * x2, y1 * y2, z1 * z2)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = Vec (fromInteger i, 0, 0)

newtype Sph a = Sph { unSph :: (a, a) }
              deriving (Show)

instance Functor Sph where
  fmap f (Sph (a, b)) = Sph (f a, f b)

findZ :: (Floating a) => a -> a -> Vec a
findZ x y =
  Vec (x, y, sqrt (1 - x**2 - y**2))

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
  l - v
  where
    n = normalise size (y, x)
    l = ((2 * dot n v)*) <$> n

toSpherical :: (Floating a, Eq a, Ord a) => Vec a -> Sph a
toSpherical (Vec (x, y, z))
  | z == 0 && x >= 0 =
    Sph (acos y, pi / 2)
  | z == 0 =
    Sph (acos y, - pi / 2)
  | z < 0 && x >= 0 =
    Sph (acos y, pi + atan (x / z))
  | z < 0 =
    Sph (acos y, - pi + atan (x / z))
  | otherwise =
    Sph (acos y, atan (x / z))

indexLatLong :: (RealFrac a, Floating a) => Int -> Int -> Sph a -> (Int, Int)
indexLatLong w h (Sph (theta, phi)) =
  ( floor $ theta / pi * fromIntegral h
  , floor $ ((phi / (2 * pi)) + 0.5) * fromIntegral w
  )
