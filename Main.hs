{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
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
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrowheads


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
data Geom2 a = Geom2 { points :: [Vec2 a], edges :: [(Vec2 a, Vec2 a)], faces:: [(Vec2 a, Vec2 a, Vec2 a)] } deriving(Eq, Show, Functor)

instance Num a => Num (Vec2 a) where
 (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
 (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

-- Rendering
-- =========

data Edge a = Edge { ebegin :: Vec2 a, eend :: Vec2 a } deriving(Show, Functor, Eq, Ord)

type Filepath = String

-- materialGray :: 
materialBlue :: Colour Double
materialBlue = sRGB24 128 203 196

materialDarkGray :: Colour Double
materialDarkGray = sRGB24 55 71 79

materialPurple :: Colour Double
materialPurple = sRGB24 171 71 188


geom2diagram :: Geom2 Double -> Diagram B
geom2diagram (Geom2 vs es fs) = 
  let draw_vs = position [(p2 (x, y), (circle 1 # fc materialDarkGray # lw none)) | (Vec2 x y) <- vs]
      draw_es = [(arrowBetween' (with & arrowHead .~ tri & tailGap .~ verySmall & headGap .~ verySmall) (sx ^& sy) (ex ^& ey)) # lc materialDarkGray | ((Vec2 sx sy), (Vec2 ex ey)) <- es]
      -- draw_fs = [trailFromVertices ([p2 (ax, ay), p2 (bx, by), p2 (cx, cy)]) # closeTrail # strokeTrail # fc materialBlue | ((Vec2 ax ay), (Vec2 bx by), (Vec2 cx cy)) <- fs]
      draw_fs = position [ ((p2 (ax, ay)), trailFromVertices ([p2 (ax, ay), p2 (bx, by), p2 (cx, cy)]) # closeTrail # strokeTrail # lw 0 # fc materialBlue) | ((Vec2 ax ay), (Vec2 bx by), (Vec2 cx cy)) <- fs]
  in draw_vs <> (mconcat draw_es) <> draw_fs # frame 10

main :: IO ()
main = do
  let w = Vec2 10 100
  let x = Vec2 10 10
  let y = Vec2 100 10
  let z = Vec2 100 100
  let wx = (w, x)
  let zw = (z, w)
  let xy = (x, y)
  let yz = (y, z)
  let zx = (z, x)
  let xyz = (z, y, x)
  return ()
  mainWith $ geom2diagram $ (Geom2 [w, x, y, z] [wx, xy, yz, zx, zw] [xyz])
-- geom2diagram (Geom2 [x, y, z] [xy, yz, xz] [xyz])
  -- im <- createMutableImage 800 600 white
  -- drawFace im ((Vec2 100 100), (Vec2  200 200),(Vec2 200 100))
  -- im <- unsafeFreezeImage im
  -- im <- drawGeom2 (Geom2 [x, y, z] [xy, yz, xz] [xyz]) 200 200 
  -- writePng "geom2.png" im
