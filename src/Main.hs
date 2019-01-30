module Main where

import           Data.Bifunctor       (Bifunctor, bimap)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           Data.Word            (Word8)
import           PFM
import           Vec

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) =
  PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (round (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) =
  PPMImage w h $ fmap clamp <$> c

bmap :: (Bifunctor f) => (a -> b) -> f a a -> f b b
bmap f = bimap f f

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

colourCorrect :: Double -> Double
colourCorrect = (+0.5) . (/2)

toCircle :: ((Int, Int) -> Vec Double) -> Int -> [[(Int, Int)]] -> [[PFMColour]]
toCircle f v m =
  fmap toCol <$> m
  where
    toCol val = if dist val <= v ^ (2 :: Int)
      then uncurry3 PFMColour (unVec $ realToFrac . colourCorrect <$> f val)
      else PFMColour 0 0 0--0.5 0.5 1.0
    dist = add . bmap ((^(2 :: Int)) . (v-))
    add (an, bn) = an + bn

circleImage :: ((Int, Int) -> Vec Double) -> Int -> PFMImage
circleImage f s = PFMImage s s $ toCircle f ((s - 1) `div` 2)
  [ [ (y, x) | x <- [0..s-1] ] | y <- reverse [0..s-1] ]

revColour :: PFMImage -> PFMImage
revColour (PFMImage w h i) =
  PFMImage w h $ reverse i

main :: IO ()
main = do
  let i = circleImage (normalise 511) 511
  BL.writeFile "normal.ppm" . encodePPM . clampImage $ i
  BL.writeFile "normal.pfm" . encode . revColour $ i
