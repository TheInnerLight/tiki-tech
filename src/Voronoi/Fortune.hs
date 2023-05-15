{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Voronoi.Fortune
  ( voronoi
  , Point (..)
  , Edge (..)
  , VoronoiPolygon(..)
  , voronoiPolygons
  , voronoiPolygonArea
  )
where

import Debug.Trace (trace, traceShow)

import Control.Arrow ((&&&), (***))

import Voronoi.BreakpointTree


import Control.Arrow ((***))

import Data.List (findIndex, findIndices, elemIndex, sortOn, nub, sortBy)

import Data.Maybe (fromJust, maybeToList, catMaybes)

import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as Map


type Index = Int

type Point = (Double, Double)


type NewPointEvent = Index
data CircleEvent   = CircleEvent Index Index Index Double Point deriving Show

--data Event a = NewPoint Index (Point a)
--           | CircleEvent Index Index Index a (Point a)
--           deriving Show

data Type = L | R deriving Show

--data Breakpoint a = Breakpoint Index Index a Type deriving Show

data IEdge = PlaceHolder | IEdge Point deriving Show
data Edge = Edge Index Index Point Point deriving (Eq, Show)

data State = State
  {
    spoints :: V.Vector Point
  , snewpointevents :: V.Vector NewPointEvent
  , scircleevents :: [CircleEvent]
  , sbreaks :: BTree
  , sedges  :: Map.Map (Index, Index) IEdge 
  , sfinaledges :: [Edge]
  , sfirst  :: Index
  , sprevd  :: Double
  } deriving Show


capEdges :: State -> State
capEdges st =
  st {sfinaledges = capEdge <$> sfinaledges st}
  where
    rangeCapX x = min 105 $ max 0 x
    rangeCapY x = min 68 $ max 0 x
    capEdge (Edge i j (x1, y1) (x2, y2)) = Edge i j (rangeCapX x1, rangeCapY y1) (rangeCapX x2, rangeCapY y2)


{- |
    Generate the voronoi diagram (defined by a set of edges) corresponding to
    the given list of centers.
-}
voronoi :: (Point, Point) -> [Point] -> [Edge]
voronoi bbox points =
  let
    go :: State -> [Edge]
    go state = if V.null (snewpointevents state) && null (scircleevents state) then
        sfinaledges $ finish bbox $ state
        --sedges state
      else
        go (nextEvent state)
  in
    go . mkState . sortOn snd $ points



-- * Private methods
-- ** Manipulating events


{- |
    > removeCEvent i j k events
    Remove a CircleEvent identified by the 3 indexes /i j k/ from /events/.
-}
removeCEvent :: Index -> Index -> Index -> [CircleEvent]
             -> [CircleEvent]
removeCEvent i j k events =
  let
    predicate (CircleEvent i' j' k' _ _) = i' == i && j' == j && k' == k
    (ls, rs) = break predicate events
  in
    if not (null rs) then ls ++ tail rs else ls

{- |
    > insertEvents newEvents events
    Inserts each Event in /newEvents/ into /events/, keeping the list sorted.
 -}
insertEvents :: [CircleEvent] -> [CircleEvent] -> [CircleEvent]
insertEvents news events =
  let
    insertEvent new events' = 
      let
        CircleEvent _ _ _ y _ = new
        (ls, rs) = span (\(CircleEvent _ _ _ y' _) -> y' < y) events'
      in
        if y /= 0 then
          ls ++ new : rs
        else
          events'
  in
    foldr insertEvent events news

-- ** Breakpoints

indexAtLeftOf :: Breakpoint -> Index
indexAtLeftOf = fst

indexAtRightOf :: Breakpoint -> Index
indexAtRightOf = snd


{- |
    > joinBreakpoints p i breaks
    Join breakpoint /i/ and /i+1/ at the point /p/. Joining two breakpoints
    results in a new breakpoint, with a corresponding new edge, and possible new
    events, as well as potentially events that need to be removed.
-}
joinBreakpoints :: Point -> Index -> Index -> Index -> Double -> Double -> BTree -> V.Vector Point
                -> (BTree
                   , [CircleEvent], [(Index, Index, Index)])
joinBreakpoints p i j k d d' breaks points =
  let
    newbreaks = joinPairAt (fst p) i j k d d' points breaks
--    newedge = edge i k (-1, -1) p
    
    prev = inOrderPredecessor (updateBreakpoint (i, j) points d') (i, j) d' points breaks
    next = inOrderSuccessor (updateBreakpoint (j, k) points d') (j, k) d' points breaks

{-
    -- TESTING
    ordered = fmap snd $ inorder breaks
    index = elemIndex (i, j) ordered
    index2 = elemIndex (j, k) ordered
    prevtest = case index of
      Nothing -> (0, 0)
      Just idx -> if idx > 0 then ordered !! (idx - 1) else (0,0)
    nexttest = case index2 of
      Nothing -> (0, 0)
      Just idx -> if idx < length ordered - 1 then ordered !! (idx + 1) else (0, 0)
-}

    (newevents, toremove)
      | prev == (0, 0) = 
        ( maybeToList $ circleEvent i k (snd next) points
        , [(i, j, k), (j, k, snd next)] )
      | next == (0, 0) =
        ( maybeToList $ circleEvent (fst prev) i k points
        , [(i, j, k), (fst prev, i, j)] )
      | otherwise = 
        ( catMaybes [circleEvent i k (snd next) points, circleEvent (fst prev) i k points]
        , [(i, j, k), (fst prev, i, j), (j, k, snd next)] )
  in 
    (newbreaks, newevents, toremove)

-- ** Processing events

{-|
   Process a NewPoint Event. It will result in a new set of breakpoints, a new
   edge, and potentially new events and events to be removed.
-}
processNewPoint :: State -> State
processNewPoint state =
  let
    idx = V.head . snewpointevents $ state
    p = V.unsafeIndex points idx
    breaks = sbreaks state
    points = spoints state
    
    -- There is a special case for the first set of breakpoints:
    firstPair = Node Nil (sfirst state, idx) $
      Node Nil (idx, sfirst state) Nil
--    firstPair = [ Breakpoint (sfirst state) idx (fst p)
--                , Breakpoint idx (sfirst state) (fst p)]
--    firstEdge = edge (sfirst state) idx (-1, -1) (-1, -1)

    -- If this is not the first pair of breakpoints:

    -- In the following lines, centerIndex is the index of the center whose
    -- parabolic section the new breakpoints land on. leftIndex and rightIndex
    -- represent the indexes of the centers of the previous and following
    -- parabolic sections to the center one, if there are any, or Nothing.

    (inserted, (j, side)) = insertPair (fst p) idx (snd p) points breaks

    updated b = updateBreakpoint b points (snd p)
    
    (next, prev) = if j == fst side then
      (side, inOrderPredecessor (updated side) side (snd p) points breaks)
    else
      (inOrderSuccessor (updated side) side (snd p) points breaks, side)
      

{-
    -- TESTING
    ordered = fmap snd $ inorder inserted
    index = elemIndex (j, idx) ordered
    index2 = elemIndex (idx, j) ordered
    prevtest = case index of
      Nothing -> (0, 0)
      Just idx -> if idx > 0 then ordered !! (idx - 1) else (0,0)
    nexttest = case index2 of
      Nothing -> (0, 0)
      Just idx -> if idx < length ordered - 1 then ordered !! (idx + 1) else (0, 0)
-}

    leftIndex   = if prev == (0, 0) then Nothing else Just $ indexAtLeftOf  $ prev
    rightIndex  = if next == (0, 0) then Nothing else Just $ indexAtRightOf $ next
    centerIndex = j


--    newEdge = edge idx centerIndex (-1, -1) (-1, -1)

    
    -- Helper function to create a circle event where the first or last index
    -- might be Nothing.
--    circleEvent' :: Maybe Index -> Index -> Maybe Index -> [Event a]
    circleEvent' i' j k' = case (i', k') of
      (Just i, Just k) -> maybeToList $ circleEvent i j k points
      _ -> []

    -- newEvents' might be a list of length 1 or 2, but should never be an empty
    -- list, as the first pair of breakpoints is  treated separately.
    newEvents' = circleEvent' leftIndex  centerIndex (Just idx) ++
                 circleEvent' (Just idx) centerIndex rightIndex

    -- toRemove :: (Maybe Index, Index, Maybe Index)
    toRemove = (leftIndex, centerIndex, rightIndex)

    sortPair a b = (min a b, max a b)
    -- Here are all the final values, which take into account wether we are in
    -- the first pair of breakpoints or not:
    newEdges
      | null breaks = Map.singleton (sortPair (sfirst state) idx) PlaceHolder
      | otherwise   = Map.insert (sortPair idx centerIndex) PlaceHolder $ sedges state

    newCircleEvents
      | null breaks = []
      | otherwise = if any (\(CircleEvent _ _ _ y _) -> y < snd p) newEvents' then error "CircleEvent at previous y" else
        insertEvents newEvents' $
          (case toRemove of
            (Just i, j, Just k) -> removeCEvent i j k
            _ -> id)  $ scircleevents state

    newBreaks
      | null breaks = firstPair
      | otherwise   = inserted

  in
    state { sbreaks = newBreaks, sedges = newEdges, scircleevents = newCircleEvents,
      snewpointevents = V.tail (snewpointevents state), sprevd = snd p}

{- |
    Process a CircleEvent Event. It will join the converging breakpoints and
    adjusts the events and edges accordingly.
-}
processCircleEvent :: State -> State
processCircleEvent state = 
  let
    (CircleEvent i j k y p):events' = scircleevents state
    breaks = sbreaks state
    points = spoints state

    -- helper function to edit Lists:
    modifyList pos ele list = let (ls,rs) = splitAt pos list in
      ls ++ ele:tail rs

    (newBreaks, newEvents', toRemove) =
      joinBreakpoints p i j k y (sprevd state + (y - sprevd state)/2) breaks points

    newEdge = IEdge p

    uncurry3 f (a,b,c) = f a b c
    newEvents = insertEvents newEvents' $
      foldr (uncurry3 removeCEvent) events' toRemove
    
    sortPair a b = (min a b, max a b)

    setVert (i, j) (edges, finaledges) = case maybeFinalEdge of
      Just (IEdge p') -> (newMap, (Edge (min i j) (max i j) p' p):finaledges)
      Nothing -> (newMap, finaledges)
      where
        (maybeFinalEdge, newMap) = Map.updateLookupWithKey updateEdge (sortPair i j) edges
        updateEdge _ PlaceHolder = Just $ IEdge p
        updateEdge _ _ = Nothing

    (newEdges', newFinalEdges) = foldr setVert (sedges state, sfinaledges state) [(i, j), (j, k)]
    newEdges = Map.insert (sortPair i k) newEdge newEdges'
  in
    state { sbreaks = newBreaks, scircleevents = newEvents, sedges = newEdges, sfinaledges = newFinalEdges, sprevd = y} 

-- ** Algorithm

{- |
    Advance the sweeping line to the next Event. Just applies the corresponding
    processing function to the next event.
-}
nextEvent :: State -> State
nextEvent state
  | V.null (snewpointevents state) && null (scircleevents state) = state
  | otherwise =
    if nextIsCircle then
      processCircleEvent state
    else
      processNewPoint state
  where
    nextPointY = (\idx -> snd $ V.unsafeIndex (spoints state) idx) $ V.head $ snewpointevents state
    nextCircleY = (\(CircleEvent _ _ _ y _) -> y) $ head $ scircleevents state
    nextIsCircle
      | V.null (snewpointevents state) = True
      | null (scircleevents state) = False
      | otherwise = nextCircleY <= nextPointY

{- |
    After finishing processing all events, we may end up with breakpoints that
    extend to infinity. This function trims those edges to a bounding box 10
    units bigger than the most extreme vertices.
-}

finish :: (Point, Point) -> State -> State
finish ((minBX, minBY),(maxX,maxY)) state
  | null (sbreaks state) = state
  | otherwise =
    let
      breaks = fmap (\x -> (updateBreakpoint x points (maxY + 20), x)) $
        inorder $ sbreaks state
      finaledges = sfinaledges state
      points = spoints state

      -- min* and max* hold the extreme values for the edges, while min*' and
      -- max*' hold those of the points. This code will figure out which way to
      -- extend the edge based on the maximum and minimum values of the points.
      -- That is to say, if for example our x value is nearest to the maximum x
      -- value of the points, then we will extend to the right (up until maxX,
      -- the maximum x value of the known edges). In the end, all vertices will
      -- be bounded to (minX, minY) (maxX, maxY) which is the original bounding
      -- box plus 20 units on each side.

      xs = (\x -> (x, x)) <$>
        concatMap (\(Edge _ _ (x, _) (x', _)) -> [x, x']) finaledges
      ys = (\x -> (x, x)) <$>
        concatMap (\(Edge _ _ (_, y) (_, y')) -> [y, y']) finaledges
      (minX, maxX) = (\(a, b) -> (a - 20, b + 20)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) xs
      (minY, maxY) = (\(a, b) -> (a - 20, b + 20)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys

      xs' = (\x -> (x, x)) <$>
        concatMap (uncurry $ flip (:) . (:[])) (V.toList points)
      ys' = (\x -> (x, x)) <$>
        concatMap (uncurry $ flip (:) . (:[])) (V.toList points)
      (minX', maxX') = (\(a, b) -> (a, b)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) xs'
      (minY', maxY') = (\(a, b) -> (a, b)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys'

      
      inRangeY b = b > minY && b < maxY
      nearest a (b, c) (d, e) = if abs (a - b) < abs (a - c)
        then d else e

      -- The guard here is to prevent trying to use the equation for a straight
      -- line in the case of a (almost) horizontal or (almost) vertical line, as
      -- the slope would be infinite. "xc" and "yc" are the "corrected" x and y
      -- value (bounded to the bounding box). We use xc if the corresponding
      -- y-value falls into rante, or yc with its corresponding x-value.

      restrict (x1,y1) (x',y')
        | abs (x1 - x') > 0.00001 && abs (y1 - y') > 0.00001 =
          if inRangeY (snd restrictX) then restrictX else restrictY
        | abs (x1 - x') <= 0.00001 =
          (x', yc)
        | otherwise =
          (xc, y')
        where
          restrictX = (xc, (xc - x1)*(y1 - y')/(x1 - x') + y1)
          restrictY = ((yc - y1)*(x1 - x')/(y1 - y') + x1, yc)
          xc = nearest x1 (maxX', minX') (maxX, minX) 
          yc = nearest y1 (maxY', minY') (maxY, minY) 

      modifyList pos ele list = let (ls,rs) = splitAt pos list in
        ls ++ ele:tail rs
      
      sortPair a b = (min a b, max a b)

      setVert (x, (i, j)) (edges, finaledges) = case maybeFinalEdge of
        Just (IEdge p') -> (newMap, (Edge (min i j) (max i j) p' (restrict p' p)):finaledges)
        Nothing -> (newMap, finaledges)
        where
          (maybeFinalEdge, newMap) = Map.updateLookupWithKey updateEdge (sortPair i j) edges
          updateEdge _ PlaceHolder = Just $ IEdge p
          updateEdge _ _ = Nothing
          p = (x, evalParabola (points `V.unsafeIndex` i) (maxY + 20) x)
    in
      state { sfinaledges = snd $ foldr setVert (sedges state, sfinaledges state) breaks }

{- |
    Create an initial state from a given set of centers.
-}
mkState :: [Point] -> State
mkState points' =
  let

    points = V.fromList points'
    sorted = sortOn (snd.snd) $
      V.foldl (\acc x -> (length acc, x):acc)  [] points
    events = V.fromList $ tail $ [0..(length sorted - 1)]
  in
    State points events [] Nil Map.empty [] (fst $ head sorted) (snd.snd $ head sorted)


-- ** Helper functions

-- | Smart constructor of Edge: it ensures that the indexes are sorted.
edge :: Index -> Index -> Point -> Point -> Edge
edge i j = Edge (min i j) (max i j) 

-- | Given three indexes and the list of points, check if the three points at
-- the indexes form a circle, and create the corresponding CircleEvent.
circleEvent :: Index -> Index -> Index
            -> V.Vector Point -> Maybe CircleEvent
circleEvent i j k points = case circle of
    Just (c@(_, y), r) -> Just $ CircleEvent i j k (y + r) c
    _ -> Nothing
  where
    circle = circleFrom3Points (points `V.unsafeIndex` i)
      (points `V.unsafeIndex` j) (points `V.unsafeIndex` k)
-- | 'evalParabola focus directrix x' evaluates the parabola defined by the
-- focus and directrix at x
evalParabola :: Point -> Double -> Double -> Double
evalParabola (fx, fy) d x = (fx*fx-2*fx*x+fy*fy-d*d+x*x)/(2*fy-2*d)

{- |
    > intersection f1 f2 d
    Find the intersection between the parabolas with focus /f1/ and /f2/ and
    directrix /d/.
-}
intersection :: Point -> Point -> Double -> Double
intersection (f1x, f1y) (f2x, f2y) d =
  let
    dist = (f1x - f2x) * (f1x - f2x) + (f1y - f2y) * (f1y-f2y)
    sqroot = sqrt $ dist * (f1y - d) * (f2y - d)
    lastterm = f1x * (d - f2y) - f2x * d
    --x1 = (f1y*f2x - sqroot + lastterm)/(f1y - f2y)
    x = (f1y*f2x + sqroot + lastterm)/(f1y - f2y)
  in
    x
    --evalParabola (f1x, f1y) d x
    --(evalParabola (f1x, f1y) d x1, evalParabola (f1x, f1y) d x2)

-- | Returns (Just) the (center, radius) of the circle defined by three given points.
-- If the points are colinear or counter clockwise, it returns Nothing.
circleFrom3Points :: Point -> Point -> Point -> Maybe (Point, Double)
circleFrom3Points (x1, y1) (x2, y2) (x3,y3) =
  let
    (bax, bay) = (x2 - x1, y2 - y1)
    (cax, cay) = (x3 - x1, y3 - y1)
    ba = bax * bax + bay * bay
    ca = cax * cax + cay * cay
    denominator = 2 * (bax * cay - bay * cax)

    x = x1 + (cay * ba - bay * ca) / denominator
    y = y1 + (bax * ca - cax * ba) / denominator
    r = sqrt $ (x-x1) * (x-x1) + (y-y1) * (y-y1)
  in
    if denominator <= 0 then
      Nothing -- colinear points or counter clockwise
    else
      Just ((x, y), r)

data VoronoiPolygon = VoronoiPolygon
  { voronoiPolygonCentre :: Point
  , voronoiEdges :: [Edge]
  , voronoiPolygonPoints :: [Point]
  } deriving (Eq, Show)

polygonFrom :: Index -> [Edge] -> VoronoiPolygon
polygonFrom i edges =
  let
    edges' = filter (\(Edge a b _ _) -> a == i || b == i) edges
    vertices = nub $ concatMap (\(Edge _ _ l r) -> [l,r]) edges'
    xs = fmap ((id &&& id) . fst) vertices
    ys = fmap ((id &&& id) . snd) vertices
    n  = fromIntegral $ length vertices
    (centerx, centery) = (/n) *** (/n) $ foldl1 (\(x, y) (a, b) -> (x+a, y+b)) vertices

    (minY, maxY) = foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys
    midY = (maxY - minY) / 2 + minY

    orderCW (ax, ay) (bx, by)
      | ax - centerx >= 0 && bx - centerx <  0 = LT
      | ax - centerx <  0 && bx - centerx >= 0 = GT
      | ax - centerx == 0 && bx - centerx == 0 = compare ay by
      | det < 0 = LT
      | det > 0 = GT
      | otherwise = compare d1 d2
      where
        det = (ax - centerx) * (by - centery) - (bx - centerx) * (ay - centery)
        d1  = (ax - centerx) * (ax - centerx) + (ay - centery) * (ay - centery)
        d2  = (bx - centerx) * (bx - centerx) + (by - centery) * (by - centery)

  in VoronoiPolygon (centerx, centery) edges (sortBy orderCW vertices)

voronoiPolygons :: (Point, Point) -> [Point] -> [VoronoiPolygon]
voronoiPolygons bbox points =
  let edges = voronoi bbox points
  in fmap (\i -> polygonFrom i edges) [0..(length points - 1)]


voronoiPolygonArea :: VoronoiPolygon -> Double
voronoiPolygonArea vp =
  sum $ fmap polygonSegmentArea $ voronoiEdges vp
  where
    (vcx, vcy) = voronoiPolygonCentre vp
    polygonSegmentArea (Edge _ _ (e1x, e1y) (e2x,e2y)) = 
      let a = sqrt ((e1x - e2x) ** 2.0 + (e1y - e2y) **2.0)
          b = sqrt ((vcx - e2x) ** 2.0 + (vcy - e2y) **2.0)
          c = sqrt ((vcx - e1x) ** 2.0 + (vcy - e1y) **2.0)
          s = 0.5*(a+b+c)
      in sqrt(s * (s-a)*(s-b)*(s-c))




{-
-- TESTING
ps = [(4.875336608745524,0.150657445690765),(-11.216506035212621,11.490726842927694),(-17.913707206936614,11.672517034976156),(15.314369189316707,16.33601558000406),(0.38035112816248784,17.775820279123977),(-11.876298872777857,18.270923221004796),(-5.012380039840515,25.160054714017036),(-9.053182555292008,30.181962786460275),(16.44086477504638,32.48880821636015)] :: [(Double, Double)]
ini = mkState ps
steps = iterate nextEvent ini
bs = fmap sbreaks steps
bs' = fmap inorder bs
bs'' d = fmap (fmap (\(_,b) -> (updateBreakpoint b (V.fromList ps) d,b))) bs'
-}
