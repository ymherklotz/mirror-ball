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

clamp :: PFMColour -> PPMColour
clamp (PFMColour ri gi bi) =
  PPMColour (f ri) (f gi) (f bi)
  where
    v s = s * 255.0
    f s = if v s > 255.0 then 255 else fromInteger (floor (v s))
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
  benchmark $ whnf (circleImage (toColour $ normalise 510)) 511
  benchmark $ whnf (circleImage (toColour $ reflect 510 (Vec (0, 0, 1)))) 511
  benchmark $ whnf (circleImage (fromImage 510 (Vec (0, 0, 1)) urban)) 511

indexTwice :: [[a]] -> (Int, Int) -> a
indexTwice c (y, x) = c !! y !! x

fromImage :: Int -> Vec Double -> PFMImage -> (Int, Int) -> PFMColour
fromImage s v (PFMImage w h c) (y, x) =
  indexTwice c . indexLatLong (w - 1) (h - 1) . toSpherical $ reflect s v (y, x)

applyGamma :: Float -> PFMImage -> PFMImage
applyGamma g (PFMImage w h c) =
  PFMImage w h $ fmap gc <$> c
  where
    gc (PFMColour r gr b) = PFMColour (gamma g r) (gamma g gr) (gamma g b)

fromImageF :: (Int, Int) -> PFMImage -> PFMColour
fromImageF (y, x) (PFMImage w h c) =
  c !! y !! x

gamma :: (Floating a) => a -> a -> a
gamma g m = m ** (1 / g)

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\_ -> putMVar mvar ())
  return mvar

printInfo :: PFMImage -> PFMImage -> PFMImage  -> PFMImage -> (Int, Int) -> IO ()
printInfo i r f fg coord = do
  let ref = reflect 510 (Vec (0, 0, 1)) coord
  let (y, x) = coord
  print coord
  putStr "  normalise: "
  print $ normalise 510 coord
  putStr "  reflect: "
  print $ ref
  putStr "  Spherical: "
  print $ toSpherical ref
  putStr "  Index: "
  print . indexLatLong 1023 511 $ toSpherical ref
  putStr "  "
  print $ fromImageF coord i
  putStr "  "
  print $ fromImageF coord r
  putStr "  "
  print $ fromImageF coord f
  putStr "  "
  print $ fromImageF coord fg

main :: IO ()
main = do
  im <- B.readFile "data/urbanEM_latlong.pfm"
  let urban = revColour $ parse im
  let i = circleImage (toColour $ normalise 510) 511
  let r = circleImage (toColour $ reflect 510 (Vec (0, 0, 1))) 511
  let f = circleImage (fromImage 510 (Vec (0, 0, 1)) urban) 511
  let fg = applyGamma 2.2 f
  createDirectoryIfMissing True "data"
  let prnti = printInfo i r f fg
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

