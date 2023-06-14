 {-# LANGUAGE ForeignFunctionInterface #-}

module Voronoi.JCVoronoi where


import Foreign.Storable
import Data.Proxy (Proxy(..))
import Foreign (Ptr, newArray, peekArray, IntPtr, free, nullPtr, malloc)
import Foreign.C (CInt)
import GHC.IO (unsafePerformIO)
import Linear (V3 (V3), cross)
import Data.Foldable (find)

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
  { jcvEdgePoint1 :: !(Double, Double)
  , jcvEdgePoint2 :: !(Double, Double)
  -- , jcvEdgeSiteIndex1 :: !Int
  -- , jcvEdgeSiteIndex2 :: !Int
  } deriving Show

data JCVPoly = JCVPoly
  { polyPoint :: !(Double, Double)
  , polyIndex :: !Int
  , polyEdges :: ![JCVEdge]
  } deriving Show

instance Storable JCVPoint where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = JCVPoint <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 
  poke ptr (JCVPoint x y) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr 8 y

instance Storable JCVRectI where
  alignment _ = 16
  sizeOf _ = 32
  peek ptr = JCVRectI <$> peekByteOff ptr 0 <*> peekByteOff ptr 16
  poke ptr (JCVRectI p1 p2) = do
    pokeByteOff ptr 0 p1
    pokeByteOff ptr 16 p2

instance Storable JCVEdgeI where
  alignment _ = 16
  sizeOf _ = 8 + 2 * 8 + 2 * 16 + 3 * 8
  peek ptr = JCVEdgeI <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16 <*> peekByteOff ptr 24 <*> peekByteOff ptr 40
  poke ptr (JCVEdgeI ne s1 s2 e1 e2) = do
    pokeByteOff ptr 0 ne
    pokeByteOff ptr 8 s1
    pokeByteOff ptr 16 s2
    pokeByteOff ptr 24 e1
    pokeByteOff ptr 40 e2

instance Storable JCVSite where
  alignment _ = 16
  sizeOf _ = 16 + 4 + 8
  peek ptr = JCVSite <$> peekByteOff ptr 0 <*> peekByteOff ptr 16 <*> peekByteOff ptr 20
  poke ptr (JCVSite p i e) = do
    pokeByteOff ptr 0 p
    pokeByteOff ptr 16 i
    pokeByteOff ptr 20 e

instance Storable JCVDiagramI where
  alignment _ = 16
  sizeOf _ = 8 + 4 + 2*16
  peek ptr = JCVDiagramI <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 28
  poke ptr (JCVDiagramI iPtr ns minP maxP) = do
    pokeByteOff ptr 0 iPtr
    pokeByteOff ptr 8 ns
    pokeByteOff ptr 12 minP
    pokeByteOff ptr 28 maxP


jcVoronoi :: [(Double, Double)] -> IO JCVoronoiDiagram
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
    pointToJCVPoint (x,y) = JCVPoint x y


jcvSites :: JCVoronoiDiagram -> IO [JCVSite]
jcvSites (JCVoronoiDiagram vPtr) = do
  sitePtr <- jcv_diagram_get_sites vPtr
  vd <- peek vPtr
  a <- peekArray (fromIntegral $ jcvDSites vd) sitePtr
  pure a


jcvSites2 :: [(Double, Double)] -> [JCVPoly]
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
        { polyPoint =  (clipPitchX $ avg $ fmap (fst . jcvEdgePoint1) es, clipPitchY $ avg $ fmap (snd . jcvEdgePoint1) es)
        , polyIndex = fromIntegral $ jcvSiteIndex site
        , polyEdges = es
        }
    e2e edge = do
      let pt1 = jcvEdgeIPoint1 edge
          pt2 = jcvEdgeIPoint2 edge
      s1 <- peek $ jcvEdgeISite1 edge
      s2 <- peek $ jcvEdgeISite1 edge
      pure $ JCVEdge { jcvEdgePoint1 = (jcvPointX pt1, jcvPointY pt1), jcvEdgePoint2 = (jcvPointX pt2, jcvPointY pt2) }
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
    (vcx, vcy) = polyPoint vp
    polygonSegmentArea (JCVEdge pt1 pt2) = 
      let (e1x, e1y) = pt1
          (e2x, e2y) = pt2
          a = sqrt ((e1x - e2x) ** 2.0 + (e1y - e2y) **2.0)
          b = sqrt ((vcx - e2x) ** 2.0 + (vcy - e2y) **2.0)
          c = sqrt ((vcx - e1x) ** 2.0 + (vcy - e1y) **2.0)
          s = 0.5*(a+b+c)
      in sqrt(s * (s-a)*(s-b)*(s-c))


lineEdgeIntersection ::  (Double, Double) -> JCVEdge -> Bool
lineEdgeIntersection  (x3, y3) edge =  
    let (x1, y1) = jcvEdgePoint1 edge
        (x2, y2) = jcvEdgePoint2 edge
        (x4, y4) = (-100, -100)
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

lineEdgeIntersection2 ::  (Double, Double) -> JCVEdge -> Bool
lineEdgeIntersection2  (x3, y3) edge =  
    let (x1, y1) = jcvEdgePoint1 edge
        (x2, y2) = jcvEdgePoint2 edge
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


pointInPoly :: (Double, Double) -> JCVPoly -> Bool
pointInPoly p poly = 
  odd $ length $ filter (lineEdgeIntersection2 p) (polyEdges poly)

findPoly :: [JCVPoly] -> (Double, Double) -> Maybe JCVPoly
findPoly polys point = 
  find (pointInPoly point) polys


