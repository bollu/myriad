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
import qualified Data.Set as S


data HalfEdge = HalfEdge { next :: Bijection, halfsize :: Int }

-- | abstract half edge object representing 
instance Show HalfEdge where
  show (HalfEdge{..}) = mconcat $ ["(" <> show i <> "->" <> show (runB next i) <> ")" | i <- [1..halfsize*2]]

-- | find the orbit of an element
orbit :: Ord a => (a -> a) -> a -> S.Set a
orbit f a = orbit_ f (S.singleton a)

orbit_ :: Ord a 
  => (a -> a) 
  -> S.Set a -- ^ current orbit set 
  -> S.Set a
orbit_ f xs = let xs' = S.union xs (S.map f xs)
  in if xs' == xs
     then xs'
     else orbit_ f xs'

orbits :: Ord a
  => (a -> a) -- ^ function
  -> S.Set a -- ^ all objects
  -> S.Set (S.Set a) -- ^ all orbits
orbits f univ = S.map (orbit f) univ

-- | get the edges of a half edge
halfedgeEdges :: HalfEdge -> S.Set (Int, Int)
halfedgeEdges HalfEdge{..} = 
  S.fromList $ [ (v, runB next v) | v <- [1..2*halfsize] ]

-- | get the faces of a half edge
halfedgeFaces :: HalfEdge -> S.Set (S.Set Int)
halfedgeFaces HalfEdge{..} = orbits (runB next) (S.fromList [1..2*halfsize])


data Bijection = Bijection { runB :: Int -> Int, runBinv :: Int -> Int }
data Vec2 a = Vec2 { vx :: a, vy :: a} deriving(Functor, Show, Eq, Ord)
data Geom2 = Geom2 { points :: [Vec2 Int], edges :: [(Vec2 Int, Vec2 Int)], faces:: [(Vec2 Int, Vec2 Int, Vec2 Int)] }

instance Num a => Num (Vec2 a) where
 (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
 (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)


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


green :: PixelRGB8
green = PixelRGB8 0 255 0

clamp :: Ord a => a -> a -> a -> a
clamp lo hi v
  | v < lo = lo
  | v > hi = hi
  | otherwise = v

-- Rendering
-- =========

-- | draw a dot with given center and radius
drawDot :: PrimMonad m 
  => MutableImage (PrimState m) PixelRGB8
  -> PixelRGB8 -- ^ color
  -> Vec2 Int  -- ^ center
  -> Int  -- ^ radius
  -> m ()
drawDot im color Vec2{..} radius = do
  let radiussq = radius * radius

  forM_ ([-radius,-radius+1..radius] :: [Int]) $ \dx -> 
    forM_ ([-radius,-radius+1..radius] :: [Int]) $ \dy -> do
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
  drawDot im green p 3

drawGeom2 :: Geom2 -> Int -> Int -> IO (Image PixelRGB8)
drawGeom2 Geom2{..} w h = do 
  im <- createMutableImage w h white
  forM_ faces $ \face -> drawFace im (orderTriCCW face)
  forM_ edges $ drawLine im
  forM_ points $ drawPoint im
  unsafeFreezeImage im

data Edge a = Edge { ebegin :: Vec2 a, eend :: Vec2 a } deriving(Show, Functor, Eq, Ord)


maxx :: Ord a => Num a => [Vec2 a] -> a
maxx as = maximum $ map vx as

minx :: Ord a => Num a => [Vec2 a] -> a
minx as = minimum $ map vx as

maxy :: Ord a => Num a => [Vec2 a] -> a
maxy as = maximum $ map vx as


miny :: Ord a => Num a => [Vec2 a] -> a
miny as = minimum $ map vx as


-- | Return signed cross product of two vctors
cross :: Num a => Vec2 a -> Vec2 a -> a
cross (Vec2 x1 y1) (Vec2 x2 y2) = x1 * y2 - x2 * y1

triArea :: Num a => Vec2 a -> Vec2 a -> Vec2 a -> a
triArea v1 v2 v3 = cross (v2 - v1) (v3 - v1)

-- bary p1 p2 p3 x returns the coordinates of x as barycentric coordinates
-- in terms of p1, p2, p3
bary :: Num a => Fractional a => Vec2 a -> Vec2 a -> Vec2 a -> Vec2 a -> (a, a, a)
bary p1 p2 p3 x = 
  let p21 = p2 - p1
      p31 = p3 - p1
      x1 = x - p1
      areaFull = cross p21 p31
      areaX2 = cross x1 p21
      areaX3 = cross x1 p31
      s = areaX2 / areaFull
      t = areaX3 / areaFull
 in (s, t, 1 - s - t)


inTri :: Ord a => Num a => Fractional a => Vec2 a -> Vec2 a -> Vec2 a -> Vec2 a -> Bool
inTri p1 p2 p3 x = 
  let p21 = p2 - p1
      p31 = p3 - p1
      x1 = x - p1
      areaFull = cross p21 p31
      areaX2 = cross x1 p21
      areaX3 = cross x1 p31
      s = areaX2 / areaFull
      t = areaX3 / areaFull
 in s >= 0 && t >= 0

-- | get the length of the edge
edgeYLen :: Num a => Edge a -> a
edgeYLen (Edge b e)  = abs $ vy e - vy b

vec2i2f :: Vec2 Int -> Vec2 Float
vec2i2f (Vec2 x y) = Vec2 (fromIntegral x) (fromIntegral y)


-- | Order triangle in CCW order
orderTriCCW :: (Num a, Ord a) => (Vec2 a, Vec2 a, Vec2 a) -> (Vec2 a, Vec2 a, Vec2 a)
orderTriCCW (a, b, c) =
 let eab = b - a
     eac = c - a
 in if cross eab eac <= 0
    then (a, c, b)
    else (a, b, c)
    

-- | pick all points in AABB, check if inside triangle using barycentric coordinates,
-- and color the point.
-- NOTE: assumes points are in CCW order
drawFace :: PrimMonad m 
 => MutableImage (PrimState m) PixelRGB8
 -> (Vec2 Int, Vec2 Int, Vec2 Int)
 -> m ()
drawFace im (v1, v2, v3) =  do
  let min_x = minx [v1, v2, v3]
  let min_y = miny [v1, v2, v3]
  let max_x = maxx [v1, v2, v3]
  let max_y = maxy [v1, v2, v3]
  forM_ [min_x..max_x] $ \x -> 
      forM_ [min_y..max_y] $ \y -> 
        if inTri (vec2i2f v1) (vec2i2f v2) (vec2i2f v3) (vec2i2f $ Vec2 x y)
           then drawDot im green (Vec2 x y) 1
           else pure ()

main :: IO ()
main = do
  im <- createMutableImage 800 600 white
  drawFace im ((Vec2 100 100), (Vec2  200 200),(Vec2 200 100))
  im <- unsafeFreezeImage im
  -- im <- drawGeom2 (Geom2 [Vec2 10 10, Vec2 500 500] [(Vec2 10 10, Vec2 500 500)]) 800 600
  writePng "geom2.png" im
