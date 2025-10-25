{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Voronoi.JCVoronoi where

import Control.Lens ((^.))

import Foreign.Storable
import Data.Proxy (Proxy(..))
import Foreign (Ptr, newArray, peekArray, IntPtr, free, nullPtr, malloc)
import Foreign.C (CInt)
import GHC.IO (unsafePerformIO)
import Linear (V3 (V3), cross, V2 (V2), _x, _y)
import Data.Foldable (find)

#include "jc_voronoi.c"

foreign import ccall "generate_from_points" generate_from_points :: Int -> Ptr JCVRectI -> Ptr JCVPoint -> IO (Ptr JCVDiagramI)
foreign import ccall "jcv_diagram_get_sites" jcv_diagram_get_sites :: Ptr JCVDiagramI -> IO (Ptr JCVSite)
foreign import ccall "jcv_diagram_free" jcv_diagram_free :: Ptr JCVDiagramI -> IO ()

data JCVoronoiDiagram = JCVoronoiDiagram (Ptr JCVDiagramI)

data JCVPoint = JCVPoint
  { jcvPointX :: !Double
  , jcvPointY :: !Double 
  } deriving Show

data JCVRectI = JCVRectI
  { jcvRPoint1 :: !JCVPoint
  , jcvRPoint2 :: !JCVPoint
  }

data JCVEdgeI = JCVEdgeI
  { jcvNextEdge :: Ptr JCVEdgeI
  , jcvEdgeISite1 :: Ptr JCVSite
  , jcvEdgeISite2 :: Ptr JCVSite
  , jcvEdgeIPoint1 :: !JCVPoint
  , jcvEdgeIPoint2 :: !JCVPoint
  } deriving Show

data JCVDiagramI = JCVDiagramI
  { jcvInternal :: !IntPtr
  , jcvDSites :: !CInt
  , jcvDMinPoint :: !JCVPoint
  , jcvDMaxPoint :: !JCVPoint
  }

data JCVSite = JCVSite 
  { jcvSitePoint :: !JCVPoint
  , jcvSiteIndex :: !CInt
  , jcvSiteEdges :: Ptr JCVEdgeI
  } deriving Show

data JCVEdge = JCVEdge
  { jcvEdgePoint1 :: !(V2 Double)
  , jcvEdgePoint2 :: !(V2 Double)
  -- , jcvEdgeSiteIndex1 :: !Int
  -- , jcvEdgeSiteIndex2 :: !Int
  } deriving Show

data JCVPoly = JCVPoly
  { polyPoint :: !(V2 Double)
  , polyIndex :: !Int
  , polyEdges :: ![JCVEdge]
  } deriving Show

instance Storable JCVPoint where
  alignment _ = (#alignment jcv_point)
  sizeOf _ = (#size jcv_point)
  peek ptr = JCVPoint <$> (#peek jcv_point, x) ptr <*> (#peek jcv_point, y) ptr
  poke ptr (JCVPoint x y) = do
    (#poke jcv_point, x) ptr x
    (#poke jcv_point, y) ptr y

instance Storable JCVRectI where
  alignment _ = (#alignment jcv_rect)
  sizeOf _ = (#size jcv_rect)
  peek ptr = JCVRectI <$> (#peek jcv_rect, min) ptr <*> (#peek jcv_rect, max) ptr
  poke ptr (JCVRectI p1 p2) = do
    (#poke jcv_rect, min) ptr p1
    (#poke jcv_rect, max) ptr p2

instance Storable JCVEdgeI where
  alignment _ = (#alignment jcv_edge)
  sizeOf _ = (#size jcv_edge)
  peek ptr = JCVEdgeI <$> (#peek jcv_edge, next) ptr <*> (#peek jcv_edge, sites[0]) ptr <*> (#peek jcv_edge, sites[1]) ptr <*> (#peek jcv_edge, pos[0]) ptr <*> (#peek jcv_edge, pos[1]) ptr
  poke ptr (JCVEdgeI ne s1 s2 e1 e2) = do
    (#poke jcv_edge, next) ptr ne
    (#poke jcv_edge, sites[0]) ptr s1
    (#poke jcv_edge, sites[1]) ptr s2
    (#poke jcv_edge, pos[0]) ptr e1
    (#poke jcv_edge, pos[1]) ptr e2

instance Storable JCVSite where
  alignment _ = (#alignment jcv_site)
  sizeOf _ = (#size jcv_site)
  peek ptr = JCVSite <$> (#peek jcv_site, p) ptr <*> (#peek jcv_site, index) ptr <*> (#peek jcv_site, edges) ptr
  poke ptr (JCVSite p i e) = do
    (#poke jcv_site, p) ptr p
    (#poke jcv_site, index) ptr i
    (#poke jcv_site, edges) ptr e

instance Storable JCVDiagramI where
  alignment _ = (#alignment jcv_diagram)
  sizeOf _ = (#size jcv_diagram)
  peek ptr = JCVDiagramI <$> (#peek jcv_diagram, internal) ptr <*> (#peek jcv_diagram, numsites) ptr <*> (#peek jcv_diagram, min) ptr <*> (#peek jcv_diagram, max) ptr
  poke ptr (JCVDiagramI iPtr ns minP maxP) = do
    (#poke jcv_diagram, internal) ptr iPtr
    (#poke jcv_diagram, numsites) ptr ns
    (#poke jcv_diagram, min) ptr minP
    (#poke jcv_diagram, max) ptr maxP


jcVoronoi :: [V2 Double] -> IO JCVoronoiDiagram
jcVoronoi items = do
  rect <- malloc
  poke rect $  JCVRectI (JCVPoint (-65) (-39)) (JCVPoint 65 39)
  let numItems = length items
  ptr <- newArray $ pointToJCVPoint <$> items
  jcvd <- JCVoronoiDiagram <$> generate_from_points numItems rect ptr
  free ptr
  free rect
  pure jcvd
  where
    pointToJCVPoint (V2 x y) = JCVPoint x y


jcvSites :: JCVoronoiDiagram -> IO [JCVSite]
jcvSites (JCVoronoiDiagram vPtr) = do
  sitePtr <- jcv_diagram_get_sites vPtr
  vd <- peek vPtr
  a <- peekArray (fromIntegral $ jcvDSites vd) sitePtr
  pure a


jcvSites2 :: [V2 Double] -> [JCVPoly]
jcvSites2 items = unsafePerformIO $  do
  jcv <- jcVoronoi items
  sites <- jcvSites jcv
  let (JCVoronoiDiagram vptr) = jcv
  polys <- traverse createPoly sites
  jcv_diagram_free vptr
  pure polys
  where 
    createPoly site = do
      edges <- siteToEdges site
      es <- traverse e2e edges
      pure JCVPoly
        { polyPoint = V2 (clipPitchX $ avg $ fmap (\j -> jcvEdgePoint1 j ^. _x) es) (clipPitchY $ avg $ fmap (\j -> jcvEdgePoint1 j ^. _y) es)
        , polyIndex = fromIntegral $ jcvSiteIndex site
        , polyEdges = es
        }
    e2e edge = do
      let pt1 = jcvEdgeIPoint1 edge
          pt2 = jcvEdgeIPoint2 edge
      s1 <- peek $ jcvEdgeISite1 edge
      s2 <- peek $ jcvEdgeISite1 edge
      pure $ JCVEdge { jcvEdgePoint1 = V2 (jcvPointX pt1) (jcvPointY pt1), jcvEdgePoint2 = V2 (jcvPointX pt2)  (jcvPointY pt2) }
    avg xs = sum xs / (fromIntegral $ length xs)
    
clipPitchX :: (Ord a, Num a, Fractional a) => a -> a
clipPitchX x = max (-52.5) $ min 52.5 x

clipPitchY :: (Ord a, Num a) => a -> a
clipPitchY y = max (-34) $ min 34 y
  

siteToEdges :: JCVSite -> IO [JCVEdgeI]
siteToEdges jcvSite = do
  go [] (jcvSiteEdges jcvSite)
  where 
    go :: [JCVEdgeI] -> Ptr JCVEdgeI -> IO [JCVEdgeI]
    go acc ptr | ptr /= nullPtr = do
      edge <- peek ptr
      go (edge : acc) (jcvNextEdge edge)
    go acc ptr = pure acc


voronoiPolygonArea :: JCVPoly -> Double
voronoiPolygonArea vp =
  sum $ fmap polygonSegmentArea $ polyEdges vp
  where
    (V2 vcx vcy) = polyPoint vp
    polygonSegmentArea (JCVEdge pt1 pt2) = 
      let (V2 e1x e1y) = pt1
          (V2 e2x e2y) = pt2
          a = sqrt ((e1x - e2x) ** 2.0 + (e1y - e2y) **2.0)
          b = sqrt ((vcx - e2x) ** 2.0 + (vcy - e2y) **2.0)
          c = sqrt ((vcx - e1x) ** 2.0 + (vcy - e1y) **2.0)
          s = 0.5*(a+b+c)
      in sqrt(s * (s-a)*(s-b)*(s-c))


lineEdgeIntersection ::  (Double, Double) -> JCVEdge -> Bool
lineEdgeIntersection  (x3, y3) edge =  
    let (V2 x1 y1) = jcvEdgePoint1 edge
        (V2 x2 y2) = jcvEdgePoint2 edge
        (V2 x4 y4) = V2 (-100) (-100)
        t =  ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4))
        u =  ((x1-x3)*(y1-y2) - (y1-y3)*(x1-x2))
        d =  (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
        ud = u*d
        td = t*d
    --print (t, u, t*d, u*d, d)
     in case signum d of
      1  -> t >= 0.0 && t <= d && u >= 0.0 && u <= d
      -1 -> t <= 0.0 && t >= d && u <= 0.0 && u >= d
      _  -> True
    --pure $ d /= 0.0 && t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0

(=~=) :: (Ord a, Fractional a, Num a) => a -> a -> Bool
(=~=) x y = abs (x - y) < 1e-7

lineEdgeIntersection2 ::  V2 Double -> JCVEdge -> Bool
lineEdgeIntersection2  (V2 x3 y3) edge =
    let (V2 x1 y1) = jcvEdgePoint1 edge
        (V2 x2 y2) = jcvEdgePoint2 edge
        d = (y1 - y2)
    in 
      if x2 =~= x3 && y2 =~= y3 then 
        True
      else
        if y2 =~= y1 && y3 =~= y1 then
          if x1 <= x3 && x3 <= x2 then True
          else if x2 <= x3 && x3 <= x1 then True
          else False
        else if(y1 < y3 && y2 >= y3 || y2 < y3 && y1 >= y3) then
          case signum d of
            -1 -> x2 * d + (y3-y2) * (x1 - x2) > x3 * d
            0  -> True
            _ ->  x2 * d + (y3-y2) * (x1 - x2) < x3 * d
        else 
          False

      
      

-- vectors :: JCVEdge -> (Double, Double) -> (V3 Double, V3 Double)
-- vectors edge (px,py) = 
--   let (x1, y1) = jcvEdgePoint1 edge
--       (x2, y2) = jcvEdgePoint2 edge
--   in (V3 (x2-x1) (y2-y1) 0, V3 (px-x1) (py-y1) 0)     

-- pointInPoly :: (Double, Double) -> JCVPoly -> Bool
-- pointInPoly p poly = 
--   all (>= 0) normals
--   where normals = map (\ edge -> uncurry cross (vectors edge p)) (polyEdges poly)

-- findPoly :: [JCVPoly] -> (Double, Double) -> Maybe JCVPoly
-- findPoly polys point = find (pointInPoly point) polys


pointInPoly :: V2 Double -> JCVPoly -> Bool
pointInPoly p poly = 
  odd $ length $ filter (lineEdgeIntersection2 p) (polyEdges poly)

findPoly :: [JCVPoly] -> V2 Double -> Maybe JCVPoly
findPoly polys point = 
  find (pointInPoly point) polys


