module Main where

import           Control.Concurrent
import           Criterion
import           Data.Bifunctor       (Bifunctor, bimap)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           Data.Word            (Word8)
import           PFM
import           System.Directory     (createDirectoryIfMissing)
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

toCircle :: ((Int, Int) -> PFMColour) -> Int -> [[(Int, Int)]] -> [[PFMColour]]
toCircle f v m =
  fmap toCol <$> m
  where
    toCol val = if dist val <= v ^ (2 :: Int)
      then f val
      else PFMColour 0 0 0--0.5 0.5 1.0
    dist = add . bmap ((^(2 :: Int)) . (v-))
    add (an, bn) = an + bn

toColour :: ((Int, Int) -> Vec Double) -> (Int, Int) -> PFMColour
toColour f val = uncurry3 PFMColour (unVec $ realToFrac . colourCorrect <$> f val)

circleImage :: ((Int, Int) -> PFMColour) -> Int -> PFMImage
circleImage f s = PFMImage s s $ toCircle f ((s - 1) `div` 2)
  [ [ (y, x) | x <- [0..s-1] ] | y <- reverse [0..s-1] ]

revColour :: PFMImage -> PFMImage
revColour (PFMImage w h i) =
  PFMImage w h $ reverse i

bench :: IO ()
bench = do
  im <- B.readFile "data/urbanEM_latlong.pfm"
  let urban = revColour $ parse im
  benchmark $ whnf (circleImage (toColour $ normalise 511)) 511
  benchmark $ whnf (circleImage (toColour $ reflect 511 (Vec (0, 0, 1)))) 511
  benchmark $ whnf (circleImage (fromImage 511 (Vec (0, 0, 1)) urban)) 511

indexTwice :: [[a]] -> (Int, Int) -> a
indexTwice c (y, x) = c !! y !! x

fromImage :: Int -> Vec Double -> PFMImage -> (Int, Int) -> PFMColour
fromImage s v (PFMImage w h c) (y, x) =
  indexTwice c . indexLatLong w h . toSpherical $ reflect s v (y, x)

applyGamma :: Float -> PFMImage -> PFMImage
applyGamma g (PFMImage w h c) =
  PFMImage w h $ fmap gc <$> c
  where
    gc (PFMColour r gr b) = PFMColour (gamma g r) (gamma g gr) (gamma g b)

gamma :: (Floating a) => a -> a -> a
gamma g m = m ** (1 / g)

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

main :: IO ()
main = do
  im <- B.readFile "data/urbanEM_latlong.pfm"
  let urban = revColour $ parse im
  let i = circleImage (toColour $ normalise 511) 511
  let r = circleImage (toColour $ reflect 511 (Vec (0, 0, 1))) 511
  let f = circleImage (fromImage 511 (Vec (0, 0, 1)) urban) 511
  let fg = applyGamma 1.7 f
  createDirectoryIfMissing True "data"
  vars <- sequence $ myForkIO <$>
    [ BL.writeFile "data/normal.ppm" . encodePPM . clampImage $ i
    , BL.writeFile "data/normal.pfm" . encode . revColour $ i
    , BL.writeFile "data/reflect.ppm" . encodePPM . clampImage $ r
    , BL.writeFile "data/reflect.pfm" . encode . revColour $ r
    , BL.writeFile "data/final_nogamma.ppm" . encodePPM . clampImage $ f
    , BL.writeFile "data/final_nogamma.pfm" . encode . revColour $ f
    , BL.writeFile "data/final.ppm" . encodePPM . clampImage $ fg
    , BL.writeFile "data/final.pfm" . encode . revColour $ fg
    ]
  sequence_ $ takeMVar <$> vars
