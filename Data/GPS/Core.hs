{-# LANGUAGE TupleSections #-}
module Data.GPS.Core
       ( -- * Types
         Distance
       , Heading
       , Speed
       , Vector
       , Trail
       , Circle
       , Arc
       , Coordinate (..)
         -- * Constants
       , north
       , south
       , east
       , west
       , radiusOfEarth
       , circumferenceOfEarth
         -- * Coordinate Functions
       , heading
       , distance
       , speed
       , getVector
       , addVector
       , getRadianPair
       , getDMSPair
       , divideArea
       , interpolate
       , circleIntersectionPoints
       , intersectionArcsOf
       , maximumDistanceOfArc
         -- * IO helpers
       , writeGPX
       , readGPX
       , readGPXSegments
         -- * Utility
       , getUTCTime
       , module Data.Geo.GPX
         ) where

import Data.Time
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad
import Text.XML.HXT.Core
import Text.XML.XSD.DateTime(DateTime,toUTCTime)
import Data.Geo.GPX
import Data.Lens.Common

class (LatL a, LonL a) => Coordinate a where
  lat :: a -> Double
  lat = runLatitude . (^. latL)
  lon :: a -> Double
  lon = runLongitude . (^. lonL)

instance Coordinate Wpt
instance Coordinate Pt

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

-- | Genearlly a circle indicates a known area in which we are searching
-- (so a center point and maximum possible distance from that point)
type Circle a = (a, Distance)

-- | An arc is represented as a circle, starting heading and ending heading
type Arc a = (Circle a, Heading, Heading)
 
type Trail a = [a]

getUTCTime :: (TimeL a) => a -> Maybe UTCTime
getUTCTime = fmap toUTCTime . (^. timeL)

distance :: (Coordinate a, Coordinate b) => a -> b -> Distance
distance x y =
  let (lat1,lon1) = getRadianPairD x
      (lat2,lon2) = getRadianPairD y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^2 + cos lat1 * cos lat2 * (sin (deltaLon / 2))^2
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c

-- | Direction two points aim toward (0 = North, pi/2 = West, pi = South, 3pi/2 = East)
heading         :: (Coordinate a, Coordinate b) => a -> b -> Heading
heading a b =
	atan2	(sin (diffLon) * cos (lat2))
		(cos(lat1) * sin (lat2) - sin(lat1) * cos lat2 * cos (diffLon))
 where
  (lat1, lon1) = getRadianPairD a
  (lat2, lon2) = getRadianPairD b
  diffLon = lon2 - lon1

getVector :: (Coordinate a, Coordinate b) => a -> b -> Vector
getVector a b = (distance a b, heading a b)

-- |Given a vector and coordinate, computes a new coordinate.
-- Within some epsilon it should hold that if
--
-- 	@dest = addVector (dist,heading) start@
--
-- then
--
-- 	@heading == heading start dest@
-- 	
-- 	@dist    == distance start dest@
addVector :: (Coordinate c) => Vector -> c -> c
addVector (d,h) p = (lonL ^= longitude (toDegrees lon2))
                  . (latL ^= latitude  (toDegrees lat2))
                  $ p
  where
	(lat,lon) = getRadianPairD p
	lat2 = asin (sin (lat) * cos (d / radiusOfEarth) + cos(lat) 
                     * sin(d/radiusOfEarth) * cos h)
        lon2 = lon - atan2 (sin h * sin (d / radiusOfEarth) * cos lat)
                           (cos (d/radiusOfEarth) - sin lat * sin lat2)

-- | Speed in meters per second, only if a 'Time' was recorded for each waypoint.
speed :: (Coordinate loc, TimeL loc, Coordinate b, TimeL b) => loc -> b -> Maybe Speed
speed a b = 
  case (getUTCTime b, getUTCTime a) of
    (Just x, Just y) -> 
      let timeDiff = realToFrac (diffUTCTime x y)
      in if timeDiff == 0 then Nothing else Just $ (distance a b) / timeDiff
    _ -> Nothing

-- |radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6378700

-- |Circumference of earth (meters)
circumferenceOfEarth :: Double
circumferenceOfEarth = radiusOfEarth * 2 * pi

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

toDegrees = (*) (180 / pi)

getRadianPairD :: (Coordinate c) => c -> (Double,Double)
getRadianPairD = (\(a,b) -> (realToFrac a, realToFrac b)) . getRadianPair

getDMSPair :: (Coordinate c) => c -> (Latitude, Longitude)
getDMSPair c = (c ^. latL, c ^. lonL)

-- |Provides a lat/lon pair of doubles in radians
getRadianPair :: (Coordinate p) => p -> (Latitude, Longitude)
getRadianPair p = (toRadians (p ^. latL), toRadians (p ^. lonL))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

-- | @interpolate c1 c2 w@ where @0 <= w <= 1@ Gives a point on the line
-- between c1 and c2 equal to c1 when @w == 0@ (weighted linearly
-- toward c2).
interpolate :: (Coordinate a) => a -> a -> Double -> a
interpolate c1 c2 w
  | w < 0 || w > 1 = error "Interpolate only works with a weight between zero and one"
  | otherwise = 
  let (h,d) = (heading c1 c2, distance c1 c2)
      v = (d * w, h)
  in addVector v c1

-- | Compute the points at which two circles intersect (assumes a flat plain).  If
-- the circles do not intersect or are identical then the result is @Nothing@.
circleIntersectionPoints :: (Coordinate a) => (a, Distance) -> (a, Distance) -> Maybe (a,a)
circleIntersectionPoints (a,r1) (b,r2)
  | a ^. latL == b ^. latL && a ^. lonL == b ^. lonL && r1 == r2 = Nothing -- FIXME need approx eq
  | r1 + r2 < ab = Nothing
  | any isNaN (map (^. latL) pts) || any isNaN (map (^. lonL) pts) = Nothing
  | otherwise = Just (p1, p2)
  where
  ab = distance a b
  angABX = acos ( (r1^2 + ab^2 - r2^2) / (2 * r1 * ab) )
  ang1 = heading a b + angABX
  ang2 = heading a b - angABX
  p1 = addVector (r1, ang1) a
  p2 = addVector (r1, ang2) a
  pts = [p1,p2]

-- | Find the area in which all given circles intersect.  The resulting
-- area is described in terms of the bounding arcs.   All cirlces must
-- intersect at two points.
intersectionArcsOf :: (Coordinate a) => [Circle a] -> [Arc a]
intersectionArcsOf cs =
  let isArcWithinCircle circ arc = maximumDistanceOfArc (fst circ) arc <= (snd circ)
      isArcWithinAllCircles arc = all ($ arc) (map isArcWithinCircle cs)
      -- getArcs :: Circle a -> Circle a -> [Arc a]
      getArcs c1 c2 = concatMap (buildArcsFromPoints c1 c2) . maybeToList $ circleIntersectionPoints c1 c2
      -- buildArcsFromPoints :: (a, a) -> [Arc a]
      buildArcsFromPoints c1 c2 (p1,p2) =
          let c1h1 = heading (fst c1) p1
              c1h2 = heading (fst c1) p2
              c2h1 = heading (fst c2) p1
              c2h2 = heading (fst c2) p2
          in [(c1,c1h1,c1h2), (c1,c1h2, c1h1), (c2,c2h1,c2h2), (c2,c2h2,c2h1)]
  in filter isArcWithinAllCircles . concatMap (uncurry getArcs) . choose2 $ cs

maximumDistanceOfArc :: (Coordinate a) => a -> Arc a -> Distance
maximumDistanceOfArc pnt ((c,r), h1, h2) =
  let pcHeading = heading pnt c
  in if ((pcHeading < h1 || pcHeading > h2) && h1 < h2) || ((pcHeading > h2 && pcHeading < h1) && h1 > h2)
         then max (distance pnt (addVector (r,h1) c)) (distance pnt (addVector (r,h2) c))
         else distance pnt c + r

choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:xs) = map (x,) xs ++ choose2 xs

-- |@divideArea vDist hDist nw se@ divides an area into a grid of equally
-- spaced coordinates within the box drawn by the northwest point (nw) and
-- southeast point (se).  Because this uses floating point there might be a
-- different number of points in some rows (the last might be too far east based
-- on a heading from the se point).
divideArea :: (Coordinate c) => Distance -> Distance -> c -> c -> [[c]]
divideArea vDist hDist nw se =
	let (top,left)  = (nw ^. latL, nw ^. lonL)
	    (btm,right) = (se ^. latL, se ^. lonL)
	    columnOne = takeWhile ( (<= west) . heading se) . iterate (addVector (vDist, south)) $ nw
	    buildRow  = takeWhile ((>= north) . heading se) . iterate (addVector (hDist, east))
	in map buildRow columnOne

-- |Reads a GPX file (using the GPX library) by simply concatenating all the
-- tracks, segments, and points ('trkpts', 'trksegs', 'trks') into a single 'Trail'.
readGPX :: FilePath -> IO (Trail Wpt)
readGPX = liftM (concatMap (^. trkptsL). concatMap (^. trksegsL) . concatMap (^. trksL)) . readGpxFile

writeGPX :: FilePath -> Trail Wpt -> IO ()
writeGPX fp ps = writeGpxFile fp $ gpx "1.0" "Haskell GPS Package (via the GPX package)" Nothing [] [] [trk Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing [trkseg ps Nothing]] Nothing

-- writeGpxFile should go in the GPX package
writeGpxFile :: FilePath -> GPX -> IO ()
writeGpxFile fp gpx = runX_ (constA gpx >>> xpickleDocument (xpickle :: PU GPX) [] fp)

runX_ t = runX t >> return ()

readGPXSegments :: FilePath -> IO [Trail Wpt]
readGPXSegments = liftM (map (concatMap (^. trkptsL)) . map (^. trksegsL) . concatMap (^. trksL)) . readGpxFile

readGpxFile :: FilePath -> IO [GPX]
readGpxFile = runX . xunpickleDocument (xpickle :: PU GPX) [withRemoveWS yes, withValidate no]
