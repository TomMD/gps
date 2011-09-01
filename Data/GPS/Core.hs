module Data.GPS.Core
       ( -- * Types
         Distance
       , Heading
       , Speed
       , Vector
       , Trail
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
       , getVector
       , addVector
       , getRadianPair
       , getDMSPair
       , divideArea
       , interpolate
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
import Control.Monad
import Text.XML.HXT.Arrow
import Text.XML.XSD.DateTime(DateTime,toUTCTime)
import Data.Geo.GPX

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

getUTCTime :: (Time a) => a -> Maybe UTCTime
getUTCTime = fmap toUTCTime . time

acos' x = if x > 1 then acos 1 else if x < (-1) then acos (-1) else acos x

distance :: (Lat a, Lon a, Lat b, Lon b) => a -> b -> Distance
distance a b =
	let x  = sin lat1 * sin lat2 + cos lat1 * cos lat2 * cos (lon2 - lon1)
	in radiusOfEarth * acos' x
 where
  (lat1, lon1) = getRadianPairD a
  (lat2, lon2) = getRadianPairD b

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
addVector :: (Lat c, Lon c) => Vector -> c -> c
addVector (d,h) p = setLon (longitudeType $ toDegrees lon2) 
                  . setLat (latitudeType $ toDegrees lat2) $ p
  where
	(lat,lon) = getRadianPairD p
	lat2 = lat + (cos h) * (d / radiusOfEarth)
	lon2 = lon - acos' ( (cos (d/radiusOfEarth) - sin lat * sin lat2) / (cos lat * cos lat2))

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

toDegrees = (*) (180 / pi)

getRadianPairD :: (Lat c, Lon c) => c -> (Double,Double)
getRadianPairD = (\(a,b) -> (realToFrac a, realToFrac b)) . getRadianPair

getDMSPair :: (Lat c, Lon c) => c -> (LatitudeType, LongitudeType)
getDMSPair c = (lat c, lon c)

-- |Provides a lat/lon pair of doubles in radians
getRadianPair :: (Lat p, Lon p) => p -> (LatitudeType, LongitudeType)
getRadianPair p = (toRadians (lat p), toRadians (lon p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

-- | @interpolate c1 c2 w@ where 0 <= w <= 1 Gives a point on the line
-- between c1 and c2 equal to @c1 when @w == 0@ (weighted linearly
-- toward c2).
interpolate :: (Lat a, Lon a) => a -> a -> Double -> a
interpolate c1 c2 w
  | w < 0 || w > 1 = error "Interpolate only works with a weight between zero and one"
  | otherwise = 
  let (h,d) = (heading c1 c2, distance c1 c2)
      v = (d * w, h)
  in addVector v c1
     
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

writeGPX :: FilePath -> Trail WptType -> IO ()
writeGPX fp ps = writeGpxFile fp $ gpx $ gpxType "1.0" "Haskell GPS Package (via the GPX package)" Nothing [] [] [trkType Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing [trksegType ps Nothing]] Nothing

-- writeGpxFile should go in the GPX package
writeGpxFile :: FilePath -> Gpx -> IO ()
writeGpxFile fp gpx = runX_ (constA gpx >>> xpickleDocument (xpickle :: PU Gpx) [] fp)

runX_ t = runX t >> return ()

readGPXSegments :: FilePath -> IO [Trail WptType]
readGPXSegments = liftM (map (concatMap trkpts) . map trksegs . concatMap trks) . readGpxFile