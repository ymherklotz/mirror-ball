module Main where

import           Data.Bifunctor       (Bifunctor, bimap)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           Data.Word            (Word8)
import           PFM

newtype Vec = Vec { unVec :: (Int, Int, Int) }

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) =
  PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (round (v s))
clamp _ = undefined

clampImage :: PFMImage -> PPMImage
clampImage (PFMImage w h c) =
  PPMImage w h . reverse $ fmap clamp <$> c

bmap :: (Bifunctor f) => (a -> b) -> f a a -> f b b
bmap f = bimap f f

toCircle :: Int -> [[(Int, Int)]] -> [[PFMColour]]
toCircle v m =
  fmap toCol <$> m
  where
    toCol val = if dist val <= v ^ (2 :: Int)
      then PFMColour 1.0 0.5 0.5
      else PFMColour 0.5 0.5 1.0
    dist = add . bmap ((^(2 :: Int)) . (v-))
    add (an, bn) = an + bn

circleImage :: Int -> PFMImage
circleImage s = PFMImage s s $ toCircle ((s - 1) `div` 2)
  [ [ (y, x) | x <- [0..s-1] ] | y <- [0..s-1] ]

main :: IO ()
main = do
  -- s <- B.readFile "/home/yannherklotz/Imperial/AdvancedGraphics/coursework1/CO417-Assignment1/UrbanProbe/urbanEM_latlong.pfm"
  s <- B.readFile "/home/yannherklotz/Downloads/memorial.pfm"
  -- BL.writeFile "random.ppm" . encodePPM . clampImage . parse $ s
  BL.writeFile "circle.ppm" . encodePPM . clampImage $ circleImage 511
