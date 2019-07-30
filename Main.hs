{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Control.Monad.Primitive
import Data.Monoid
import Codec.Picture.Types
import Codec.Picture
import Debug.Trace


data HalfEdge = HalfEdge { next :: Bijection, halfsize :: Int }
instance Show HalfEdge where
  show (HalfEdge{..}) = mconcat $ ["(" <> show i <> "->" <> show (runB next i) <> ")" | i <- [1..halfsize*2]]

data Bijection = Bijection { runB :: Int -> Int, runBinv :: Int -> Int }
data Vec2 a = Vec2 { vx :: a, vy :: a} deriving(Functor)
data Geom2 = Geom2 { points :: [Vec2 Int], lines :: [(Vec2 Int, Vec2 Int)] }

fvec2 :: Vec2 Int -> Vec2 Float
fvec2 = fmap fromIntegral

generateImage' :: Int -> Int -> (Int -> Int -> PixelRGB8) -> Image PixelRGB8
generateImage' w h f = generateImage f w h

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 20 20 20

gray :: PixelRGB8
gray = PixelRGB8 100 100 100

clamp :: Ord a => a -> a -> a -> a
clamp lo hi v
  | v < lo = lo
  | v > hi = hi
  | otherwise = v

-- | draw a dot with given center and radius
drawDot :: PrimMonad m 
  => MutableImage (PrimState m) PixelRGB8
  -> PixelRGB8 -- ^ color
  -> Vec2 Int  -- ^ center
  -> Int  -- ^ radius
  -> m ()
drawDot im color Vec2{..} radius = do
  let radiussq = radius * radius

  forM_ [-radius,-radius+1..radius] $ \dx -> 
    forM_ [-radius,-radius+1..radius] $ \dy -> do
      if dx * dx + dy * dy >= radiussq
      then pure ()
      else do
        let x = clamp 0 (mutableImageWidth im - 1) $ vx + dx
        let y = clamp 0 (mutableImageHeight im - 1) $ vy + dy
        writePixel im x y color
        


drawPoint :: PrimMonad m => 
  MutableImage (PrimState m) PixelRGB8
  -> Vec2 Int
  -> m ()
drawPoint im v = drawDot im black v 6

lerp :: Float -> Vec2 Float -> Vec2 Float -> Vec2 Float
lerp t (Vec2 x y) (Vec2 x' y') = Vec2 ((1 - t)*x + t*x') ((1-t)*y + t*y')

drawLine :: PrimMonad m 
  => MutableImage (PrimState m) PixelRGB8 
  -> (Vec2 Int, Vec2 Int)
  -> m ()
drawLine im (v1, v2) = let n = 1000 in forM_ [0,1..n] $ \i -> do
  let t = fromIntegral i / fromIntegral n
  let p = fmap floor $ lerp t (fvec2 v1) (fvec2 v2)
  drawDot im gray p 3

-- http://hackage.haskell.org/package/JuicyPixels-3.3.3.1/docs/Codec-Picture-Types.html#t:MutableImage

drawGeom2 :: Geom2 -> Int -> Int -> IO (Image PixelRGB8)
drawGeom2 Geom2{..} w h = do 
  im <- createMutableImage w h white
  forM_ lines $ drawLine im
  forM_ points $ drawPoint im
  unsafeFreezeImage im

main :: IO ()
main = do
  im <- drawGeom2 (Geom2 [Vec2 10 10, Vec2 500 500] [(Vec2 10 10, Vec2 500 500)]) 800 600
  writePng "geom2.png" im
