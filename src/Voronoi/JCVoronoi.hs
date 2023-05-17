 {-# LANGUAGE ForeignFunctionInterface #-}

module Voronoi.JCVoronoi where


import Foreign.Storable
import Data.Proxy (Proxy(..))
import Foreign (Ptr, newArray, peekArray, IntPtr, free, nullPtr, malloc)
import Foreign.C (CInt)
import GHC.IO (unsafePerformIO)

foreign import ccall "generate_from_points" generate_from_points :: Int -> Ptr JCVRect -> Ptr JCVPoint -> IO (Ptr JCVDiagram)
foreign import ccall "jcv_diagram_get_sites" jcv_diagram_get_sites :: Ptr JCVDiagram -> IO (Ptr JCVSite)
foreign import ccall "jcv_diagram_free" jcv_diagram_free :: Ptr JCVDiagram -> IO ()

data JCVoronoiDiagram = JCVoronoiDiagram (Ptr JCVDiagram)

data JCVPoint = JCVPoint
  { jcvPointX :: Double
  , jcvPointY:: Double 
  } deriving Show

data JCVRect = JCVRect
  { jcvRPoint1 :: JCVPoint
  , jcvRPoint2 :: JCVPoint
  }

data JCVEdge = JCVEdge
  { jcvNextEdge :: Ptr JCVEdge
  , jcvEdgePoint1 :: JCVPoint
  , jcvEdgePoint2 :: JCVPoint
  }

data JCVDiagram = JCVDiagram
  { jcvInternal :: IntPtr
  , jcvDSites :: CInt
  , jcvDMinPoint :: JCVPoint
  , jcvDMaxPoint :: JCVPoint
  }

data JCVSite = JCVSite 
  { jcvSitePoint :: JCVPoint
  , jcvSiteIndex :: CInt
  , jcvSiteEdges :: Ptr JCVEdge
  } deriving Show


data JCVPoly = JCVPoly
  { polyPoint :: (Double, Double)
  , polyIndex :: Int
  , polyEdges :: [((Double, Double),(Double, Double))]
  } deriving Show

instance Storable JCVPoint where
  alignment _ = 8
  sizeOf _ = 16
  peek ptr = JCVPoint <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 
  poke ptr (JCVPoint x y) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr 8 y

instance Storable JCVRect where
  alignment _ = 16
  sizeOf _ = 32
  peek ptr = JCVRect <$> peekByteOff ptr 0 <*> peekByteOff ptr 16
  poke ptr (JCVRect p1 p2) = do
    pokeByteOff ptr 0 p1
    pokeByteOff ptr 16 p2

instance Storable JCVEdge where
  alignment _ = 16
  sizeOf _ = 8 + 16 + 2 * 16 + 3 * 8
  peek ptr = JCVEdge <$> peekByteOff ptr 0 <*> peekByteOff ptr 24 <*> peekByteOff ptr 40
  poke ptr (JCVEdge ne e1 e2) = do
    pokeByteOff ptr 0 ne
    pokeByteOff ptr 24 e1
    pokeByteOff ptr 32 e2

instance Storable JCVSite where
  alignment _ = 16
  sizeOf _ = 16 + 4 + 8
  peek ptr = JCVSite <$> peekByteOff ptr 0 <*> peekByteOff ptr 16 <*> peekByteOff ptr 20
  poke ptr (JCVSite p i e) = do
    pokeByteOff ptr 0 p
    pokeByteOff ptr 16 i
    pokeByteOff ptr 20 e

instance Storable JCVDiagram where
  alignment _ = 16
  sizeOf _ = 8 + 4 + 2*16
  peek ptr = JCVDiagram <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 28
  poke ptr (JCVDiagram iPtr ns minP maxP) = do
    pokeByteOff ptr 0 iPtr
    pokeByteOff ptr 8 ns
    pokeByteOff ptr 12 minP
    pokeByteOff ptr 28 maxP


jcVoronoi :: [(Double, Double)] -> IO JCVoronoiDiagram
jcVoronoi items = do
  rect <- malloc
  poke rect $  JCVRect (JCVPoint 0 0) (JCVPoint 105 68)
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
  print (fromIntegral $ jcvDSites vd)
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
      let es = fmap (\(p1, p2) -> (p2p p1, p2p p2)) $ fmap e2e edges
      pure JCVPoly
        { polyPoint =  (avg $ fmap (fst . fst) es, avg $ fmap (snd . fst) es)
        , polyIndex = fromIntegral $ jcvSiteIndex site
        , polyEdges = es
        }
    e2e edge =
      (jcvEdgePoint1 edge, jcvEdgePoint2 edge)
    p2p point = (jcvPointX point, jcvPointY point)
    avg xs = sum xs / (fromIntegral $ length xs)
    

  

siteToEdges :: JCVSite -> IO [JCVEdge]
siteToEdges jcvSite = do
  go [] (jcvSiteEdges jcvSite)
  where 
    go :: [JCVEdge] -> Ptr JCVEdge -> IO [JCVEdge]
    go acc ptr | ptr /= nullPtr = do
      edge <- peek ptr
      go (edge : acc) (jcvNextEdge edge)
    go acc ptr = pure acc


voronoiPolygonArea :: JCVPoly -> Double
voronoiPolygonArea vp =
  sum $ fmap polygonSegmentArea $ polyEdges vp
  where
    (vcx, vcy) = polyPoint vp
    polygonSegmentArea ((e1x, e1y),(e2x,e2y)) = 
      let a = sqrt ((e1x - e2x) ** 2.0 + (e1y - e2y) **2.0)
          b = sqrt ((vcx - e2x) ** 2.0 + (vcy - e2y) **2.0)
          c = sqrt ((vcx - e1x) ** 2.0 + (vcy - e1y) **2.0)
          s = 0.5*(a+b+c)
      in sqrt(s * (s-a)*(s-b)*(s-c))



