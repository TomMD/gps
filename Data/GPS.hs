{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, EmptyDataDecls, BangPatterns, TupleSections #-}
-- |A basic GPS library with calculations for distance and speed along
-- with helper functions for filtering/smoothing trails.  All distances
-- are in meters and time is in seconds.  Speed is thus meters/second

module Data.GPS
       ( -- * Types
         Distance
       , Heading
       , Speed
       , Vector
       , Trail
       , AvgMethod(..)
       , PointGrouping(..)
       , TrailProcessing(..)
         -- * Constants
       , north
       , south
       , east
       , west
       , radiusOfEarth
         -- * Coordinate Functions
       , heading
       , distance
       , speed
       , addVector
       , getRadianPair
       , getDMSPair
       , divideArea
         -- * Trail Functions
       , totalDistance
       , avgSpeeds
       , slidingAverageSpeed
       , transformTrail
       , groupPoints
       , restLocations
       , closestDistance
       , filterByMaxSpeed
       , convexHull
         -- * Other helpers
       , readGPX
       , readGPXSegments
       , module Data.Geo.GPX
       ) where

import Data.Function (on)
import Data.Ord (comparing)
import Data.List as L (sort, mapAccumL, minimumBy, maximumBy, sortBy)
import Data.Maybe
import Data.Geo.GPX hiding (none, cmt)
import Text.Show.Functions ()

import Text.XML.HXT.Arrow
import Text.XML.XSD.DateTime(DateTime, toUTCTime)

import Data.Time
import Data.Maybe (listToMaybe)
import Data.Fixed
import Control.Monad

-- These modules are used for filtering speeds and other trail features
import Statistics.Function as F
import Statistics.Sample
import qualified Data.Vector.Unboxed as V

-- |Distances are expressed in meters
type Distance = Double

-- |Angles are expressed in radians from North.
-- 	0	== North
-- 	pi/2	== West
-- 	pi 	== South
-- 	(3/2)pi	== East    == - (pi / 2)
type Heading = Double

-- |Speed is hard coded as meters per second
type Speed = Double
type Vector = (Distance, Heading)

type Trail a = [a]

getUTCTime :: (Lat a, Lon a, Time a) => a -> Maybe UTCTime
getUTCTime = fmap toUTCTime . time

distance :: (Lat a, Lon a, Lat b, Lon b) => a -> b -> Distance
distance a b =
	let x  = sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos (lon2 - lon1)
	    x' = if x > 1 then 1 else x
	in radiusOfEarth * acos x'
 where
  (lat1, lon1) = getRadianPairD a
  (lat2, lon2) = getRadianPairD b

-- | Find the total distance traveled
totalDistance :: (Lat a, Lon a) => [a] -> Distance
totalDistance as = sum $ zipWith distance as (drop 1 as)

takeWhileEnd :: (a -> Bool) -> [a] -> (Maybe a, [a],[a])
takeWhileEnd p xs = go xs Nothing
  where
  go [] e = (e, [], [])
  go (a:as) e
    | p a = let (e',xs,zs) = go as (Just a) in (e',a:xs,zs)
    | otherwise = (e,[], a:as)

data AvgMethod
  = AvgMean              -- ^ Obtain the 'mean' of the considered points
  | AvgHarmonicMean      -- ^ Obtain the 'harmonicMean'
  | AvgGeometricMean     -- ^ Obtain the 'geometricMean'
  | AvgMedian            -- ^ Obtain the median of the considered points
  | AvgEndPoints         -- ^ Compute the speed considering only the given endpoints
  | AvgMinOf [AvgMethod] -- ^ Take the minimum of the speeds from the given methods
  | AvgWith ([(LatitudeType,LongitudeType)] -> Speed)
    
-- | @avgSpeeds n points@
-- Average speed using a window of up to @n@ seconds and averaging by taking the
-- Median ('AvgMedian').
avgSpeeds :: (Lat a, Lon a, Time a) => NominalDiffTime -> Trail a -> [(UTCTime, Speed)]
avgSpeeds = slidingAverageSpeed AvgHarmonicMean

-- | @slidingAverageSpeed m n@ Average speed using a moving window of up to @n@ seconds
-- and an 'AvgMethod' of @m@.
slidingAverageSpeed :: (Lat a, Lon a, Time a) => 
                       AvgMethod -> NominalDiffTime -> Trail a -> [(UTCTime, Speed)]
slidingAverageSpeed _ _ [] = []
slidingAverageSpeed m n (x:xs) =
    let avg = getAvg (x:xs') m
        avgTime = getAvgTime x (fromMaybe x e)
    in case avgTime of
	Nothing -> []
        Just t  -> (t,avg) : slidingAverageSpeed m n xs
  where
  (e,xs',rest) = takeWhileEnd (\c -> getTimeDiff c x <= Just n) xs
  getTimeDiff a b = on (liftM2 diffUTCTime) getUTCTime a b
  
  --  getAvg :: [] -> AvgMethod -> Speed
  getAvg cs AvgMean =
    let ss = getSpeedsV cs
    in case m of
        AvgMean -> mean ss
        AvgHarmonicMean  -> harmonicMean ss
        AvgGeometricMean -> geometricMean ss
        AvgMedian ->
          let ss' = F.sort $ getSpeedsV cs
              len = V.length ss'
              mid = len `div` 2
          in if V.length ss' < 3
             then mean ss'
             else if odd len then ss' V.! mid else mean (V.slice mid 2 ss')
        AvgEndPoints -> fromMaybe 0 . join . fmap (speed x) $ e
        AvgMinOf as -> minimum $ map (getAvg cs) as
        AvgWith f -> f (map getDMSPair cs)
  getAvgTime a b = liftM2 addUTCTime (getTimeDiff b a) (getUTCTime a)
  getSpeedsV = V.fromList . getSpeeds
  getSpeeds zs = concatMap (maybeToList . uncurry speed) $ zip zs (drop 1 zs)

-- | Direction two points aim toward (0 = North, pi/2 = West, pi = South, 3pi/2 = East)
heading         :: (Lat a, Lon a, Lat b, Lon b) => a -> b -> Heading
heading a b =
	atan2	(sin (diffLon) * cos (lat2)) 
		(cos(lat1) * sin (lat2) - sin(lat1) * cos lat2 * cos (diffLon))
 where
  (lat1, lon1) = getRadianPairD a
  (lat2, lon2) = getRadianPairD b
  diffLon = lon1 - lon2

getVector :: (Lat a, Lon a, Lat b, Lon b) => a -> b -> Vector
getVector a b = (distance a b, heading a b)

-- | Speed in meters per second, only if a 'Time' was recorded for each waypoint.
speed :: (Lat loc, Lon loc, Time loc, Lat b, Lon b, Time b) => loc -> b -> Maybe Speed
speed a b = 
  case (getUTCTime b, getUTCTime a) of
    (Just x, Just y) -> 
      let timeDiff = realToFrac (diffUTCTime x y)
      in if timeDiff == 0 then Nothing else Just $ (distance a b) / timeDiff
    _ -> Nothing

-- |radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6378700

-- |North is 0 radians
north :: Heading
north = 0

-- |South, being 180 degrees from North, is pi.
south :: Heading
south = pi

-- |East is 270 degrees (3 pi / 2)
east :: Heading
east = (3 / 2) * pi

-- |West is 90 degrees (pi/2)
west :: Heading
west = pi / 2

toDecimal = (*) (180 / pi)


-- |Given a vector and coordinate, computes a new coordinate.
-- Within some epsilon it should hold that if
--
-- 	@dest = addVector (dist,heading) start@
--
-- then
--
-- 	@heading == dmsHeading start dest@
-- 	
-- 	@dist    == distance start dest@
addVector :: (Lat c, Lon c) => Vector -> c -> c
addVector (d,h) p = setLon (longitudeType lon2) . setLat (latitudeType lat2) $ p
  where
	(lat,lon) = getRadianPairD p
	lat2 = lat + (cos h) * (d / radiusOfEarth)
	lon2 = lon + acos ( (cos (d/radiusOfEarth) - sin lat * sin lat2) / (cos lat * cos lat2))

getRadianPairD :: (Lat c, Lon c) => c -> (Double,Double)
getRadianPairD = (\(a,b) -> (realToFrac a, realToFrac b)) . getRadianPair

getDMSPair :: (Lat c, Lon c) => c -> (LatitudeType, LongitudeType)
getDMSPair c = (lat c, lon c)

-- |Provides a lat/lon pair of doubles in radians
getRadianPair :: (Lat p, Lon p) => p -> (LatitudeType, LongitudeType)
getRadianPair p = (toRadians (lat p), toRadians (lon p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

data TrailProcessing
     = LinearTime
     | MaxSpeed Speed
     | ElimRestLocations Distance NominalDiffTime
     | BezierCurve PointGrouping
     deriving (Show)
              
data PointGrouping
  = BetweenSpeeds Speed Speed  -- ^ Groups trail segments into
                               -- contiguous points within the speed
                               -- and all others outside of the speed.
                               -- The "speed" from point p(i) to p(i+1) is
                               -- associated with p(i) (execpt for the
                               -- first speed value, which is
                               -- associated with both the first and
                               -- second point)
  | RestPoint Distance NominalDiffTime -- ^ A "rest point" means the
                                       -- coordinates were within a
                                       -- given distance for at least
                                       -- a particular amount of time.
  | SpansTime NominalDiffTime -- ^ Perhaps the most trivial grouping, chunking points into groups spanning at most the given time interval
  | IntersectionOf [PointGrouping] -- ^ intersects the given groupings
  | GroupBy ( [(LatitudeType,LongitudeType)] -> [[(LatitudeType,LongitudeType)]] )
    deriving (Show)
             
-- Grouping point _does not_ result in deleted points. It is always true that:
--
--     concat (groupPoints x ts) == ts -- forall x
--
-- The purpose of grouping is usually for later processing.  Any desire to drop
-- points that didn't meet a particular grouping criteria can be filled with
-- a composition with 'filter' (or directly via 'filterPoints'.
groupPoints :: (Lat a, Lon a, Time a) => PointGrouping -> Trail a -> [Trail a]
groupPoints (BetweenSpeeds low hi) ps =
  let spds = concatMap maybeToList $ zipWith speed ps (drop 1 ps)
      psSpds = [(p,s) | p <- ps, s <- maybeToList (listToMaybe spds) ++ spds]
      inRange x = x >= low && x <= hi
      chunk [] = []
      chunk xs@(x:_) =
        let op = if inRange (snd x) then span else break
            (r,rest) = op (inRange . snd) xs
        in r : chunk xs
  in map (map fst) $ chunk psSpds
groupPoints (RestPoint d s) ps =
  let consToFirst x [] = [[x]]
      consToFirst x (a:as) = (x:a) : as
      go [] nonRests = [reverse nonRests]
      go (a:as) nonRests =
        case takeWhileLast ((<=) d . distance a) as of
          (Just l, close, far) ->
            case (getUTCTime a, getUTCTime l) of
              (Just t1, Just t2) ->
                let diff = diffUTCTime t2 t1
                in if diff >= s then reverse nonRests : (a:close) : go far [] else go as (a:nonRests)
              _ -> consToFirst a $ go as nonRests
          _ -> consToFirst a $ go as nonRests
  in go ps []
groupPoints (SpansTime n) ps =
  let times  = mkTimePair ps
      chunk [] = []
      chunk (x:xs) =
        let (good,rest) = span ((<= addUTCTime n (snd x)) . snd) xs in good : chunk rest
  in map (map fst) $ chunk times
groupPoints (IntersectionOf gs) ps =
  let groupings = map (flip groupPoints ps) gs
      dropExact :: Int -> [Int] -> [Int]
      dropExact i [] = []
      dropExact i [x]
        | x <= i = []
        | True   = [x - i]
      dropExact i (x:y:xs)
        | x > i = x - i : y : xs
        | True  = dropExact (i-x) (y:xs)
      -- chunk :: [[Int]] -> pnts -> [pnts]
      chunk _ [] = []
      chunk lens xs = 
        let minLen = max 1 . minimum . concatMap (take 1) $ lens
            (c,rest) = splitAt minLen xs
        in c : chunk (filter (not . null) $ map (dropExact minLen) lens) rest
  in chunk (map (map length) groupings) ps
groupPoints (GroupBy f) ps =
  let xs = map length $ f (map getDMSPair ps)
      chunk [] [] = []
      chunk [] vs = [vs]
      chunk (l:ls) vs =
        let (h,rest) = splitAt l vs in h : chunk ls rest
  in chunk xs ps

filterPoints :: (Lat a, Lon a, Time a) => PointGrouping -> Trail a -> [Trail a]
filterPoints = error "FIXME, implement me"

mkTimePair :: (Lat a, Lon a, Time a) => Trail a -> [(a,UTCTime)]
mkTimePair xs =
  let timesM = map (\x-> fmap (x,) $ getUTCTime x) xs
  in concatMap maybeToList timesM

transformToBezierCurve :: (Lat a, Lon a, Time a) => Trail a -> Trail a
transformToBezierCurve xs = 
  let times = mkTimePair xs
      end = last times
      top = head times
      sndDiff = diffUTCTime `on` snd
      totalTime  = sndDiff end top
      queryTimes = [fromTo (sndDiff end t / totalTime) | t <- times]
      fromTo = fromRational . toRational
  in if null times
      then xs
      else map (bezierPoint xs) queryTimes
  where
    
bezierPoint :: (Lat a, Lon a) => [a] -> Double -> a
bezierPoint []   _ = error "Can not create a bezier point from an empty list"
bezierPoint [p0] _ = p0
bezierPoint ps t   = interpolate (bezierPoint (init ps) t) (bezierPoint (tail ps) t) t
    
-- | @interpolate c1 c2 w@ where 0 <= w <= 1 Gives a point on the line
-- between c1 and c2 equal to @c1 when @w == 0@ (weighted linearly
-- toward c2).
interpolate :: (Lat a, Lon a) => a -> a -> Double -> a
interpolate c1 c2 w =
  let (h,d) = (heading c1 c2, distance c1 c2)
      v = (h, d * (1 - w))
  in addVector v c1

transformTrail :: (Lat a, Lon a, Time a) => TrailProcessing -> Trail a -> Trail a
transformTrail LinearTime ts = linearTime ts
transformTrail (MaxSpeed x) ts = filterByMaxSpeed x ts
transformTrail (BezierCurve grp) ts = concatMap transformToBezierCurve (groupPoints grp ts)

-- |Filters out any points that go backward in time (thus must not be valid if this is a trail)
linearTime :: (Lon a, Lat a, Time a) => [a] -> [a]
linearTime [] = []
linearTime (p:ps) = go (getUTCTime p) ps
  where
  go _ [] = []
  go t (p:ps) = if getUTCTime p < t then go t ps else p : go (getUTCTime p) ps

-- |Filter out all points that result in a speed greater than a given
-- value (the second point is dropped)
filterByMaxSpeed :: (Lat loc, Lon loc, Time loc) => Speed -> Trail loc -> Trail loc
filterByMaxSpeed mx xs =
	let ss = zipWith speed xs (drop 1 xs)
	    fs = filter ((< Just mx) . fst) (zip ss $ drop 1 xs)
	in take 1 xs ++ map snd fs

-- |Creates a list of trails all of which are within the given distance of each
-- other spanning atleast the given amount of time.
--
-- For example @restLocations 50 600@
-- would return lists of all points that are within 50 meters of each other and
-- span at least 10 minutes (600 seconds).
--
-- Note this gives points within fifty meters of the earliest point - wandering
-- in a rest area with a 50 meter radius could result in several rest points
-- ([a,b..]) or even none if the distance between individual points exceeds 50m.
restLocations :: (Lat a, Lon a, Time a) => Distance -> NominalDiffTime -> Trail a -> [Trail a]
restLocations d s xs = go xs
  where
  go [] = []
  go (a:as) =
	case takeWhileLast ((<=) d . distance a) as of
		(Just l, close, far) ->
			case (getUTCTime a, getUTCTime l) of
				(Just t1, Just t2) ->
					let diff = diffUTCTime t2 t1
					in if diff >= s then (a:close) : go far else go as
				_ -> go as
		_ -> go as

takeWhileLast :: (a -> Bool) -> [a] -> (Maybe a, [a], [a])
takeWhileLast p [] = (Nothing, [], [])
takeWhileLast p (x:xs)
	| not (p x) = (Nothing, [], x:xs)
	| otherwise = go x xs
  where
  go !a [] = (Just a, [], [])
  go !a (b:bs)
	| p b = let (c,d,f) = go b bs in (c, a:d, f)
	| otherwise = (Just a, [], b:bs)

-- |Returns the closest distance between two trails (or Nothing if a trail is empty)
-- O( (n * m) * log (n * m) )
closestDistance :: (Lat a, Lon a) => Trail a -> Trail a -> Maybe Distance
closestDistance as bs = listToMaybe $ L.sort [distance a b | a <- as, b <- bs]


-- |@divideArea vDist hDist nw se@ divides an area into a grid of equally
-- spaced coordinates within the box drawn by the northwest point (nw) and
-- southeast point (se).  Because this uses floating point there might be a
-- different number of points in some rows (the last might be too far east based
-- on a heading from the se point).
divideArea :: (Lat c, Lon c) => Distance -> Distance -> c -> c -> [[c]]
divideArea vDist hDist nw se =
	let (top,left)  = (lat nw, lon nw)
	    (btm,right) = (lat se, lon se)
	    columnOne = takeWhile ( (<= west) . heading se) . iterate (addVector (vDist, south)) $ nw
	    buildRow  = takeWhile ((>= north) . heading se) . iterate (addVector (hDist, east))
	in map buildRow columnOne

-- |Reads a GPX file (using the GPX library) by simply concatenating all the
-- tracks, segments, and points ('trkpts', 'trksegs', 'trks') into a single 'Trail'.
readGPX :: FilePath -> IO (Trail WptType)
readGPX = liftM (concatMap trkpts . concatMap trksegs . concatMap trks) . readGpxFile

readGPXSegments :: FilePath -> IO [Trail WptType]
readGPXSegments = liftM (map (concatMap trkpts) . map trksegs . concatMap trks) . readGpxFile

-- | Uses Grahams scan to compute the convex hull of the given points.
-- This operation requires sorting of the points, so don't try it unless
-- you have notably more memory than the list of points will consume.
convexHull :: (Eq c, Lat c, Lon c) => [c] -> [c]
convexHull xs =
	let first = southMost xs
	in case first of
		Nothing -> []
		Just f  ->
	    	     let sorted = sortBy (comparing (eastZeroHeading f)) (filter (/= f) xs)
		     in case sorted of
			(a:b:cs) -> grahamScan (b:a:f:[]) cs
			cs       -> f : cs
  where
  grahamScan [] _ = []
  grahamScan ps [] = ps
  grahamScan (x:[]) _ = [x]
  grahamScan (p2:p1:ps) (x:xs) =
	case turn p1 p2 x of
		LeftTurn  -> grahamScan (x:p2:p1:ps) xs
		Straight  -> grahamScan (x:p2:p1:ps) xs
		_	  -> grahamScan (p1:ps) (x:xs)

eastZeroHeading :: (Lat c, Lon c) => c -> c -> Heading
eastZeroHeading s = (`mod'` (2*pi)) . (+ pi/2) . heading s

data Turn = LeftTurn | RightTurn | Straight deriving (Eq, Ord, Show, Read, Enum)

turn :: (Lat c, Lon c) => c -> c -> c -> Turn
turn a b c =
	let h1 = eastZeroHeading a b
	    h2 = eastZeroHeading b c
	    d  = h2 - h1
	in if d >= 0 && d < pi then LeftTurn else RightTurn

-- | Find the southmost point
southMost :: (Lat c) => [c] -> Maybe c
southMost []  = Nothing
southMost cs = Just . minimumBy (comparing lat) $ cs
