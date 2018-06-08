{-# LANGUAGE TupleSections #-}
module Geo.Computations.Basic
       ( -- * Types
         Distance
       , Heading
       , Speed
       , Vector
       , Trail
       , Circle
       , Arc
       , Point(..)
       , pt
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
       , interpolate
       , circleIntersectionPoints
       , intersectionArcsOf
       , maximumDistanceOfArc
       ) where

import Data.Time
import Data.Maybe
import Geo.Types

-- | Distances are expressed in meters
type Distance = Double

-- | Angles are expressed in radians from North.
--      0       == North
--      pi/2    == West
--      pi      == South
--      (3/2)pi == East    == - (pi / 2)
type Heading = Double

-- | Speed is hard-coded as meters per second
type Speed = Double
type Vector = (Distance, Heading)

-- | Generally a circle indicates a known area in which we are searching
-- (so a center point and maximum possible distance from that point)
type Circle a = (a, Distance)

-- | An arc is represented as a circle, starting heading and ending heading
type Arc a = (Circle a, Heading, Heading)
 
type Trail a = [a]

distance :: Point -> Point -> Distance
distance x y =
  let (lat1,lon1) = getRadianPair x
      (lat2,lon2) = getRadianPair y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^(2::Int) + cos lat1 * cos lat2 * (sin (deltaLon / 2))^(2::Int)
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c

-- | Direction two points aim toward (0 = North, pi/2 = West, pi = South, 3pi/2 = East)
heading         :: Point -> Point -> Heading
heading a b =
        atan2   (sin (diffLon) * cos (lat2))
                (cos(lat1) * sin (lat2) - sin(lat1) * cos lat2 * cos (diffLon))
 where
  (lat1, lon1) = getRadianPair a
  (lat2, lon2) = getRadianPair b
  diffLon = lon2 - lon1

getVector :: Point -> Point -> Vector
getVector a b = (distance a b, heading a b)

-- | Given a vector and coordinate, computes a new coordinate.
-- Within some epsilon it should hold that if
--
--      @dest = addVector (dist,heading) start@
--
-- then
--
--      @heading == heading start dest@
--      
--      @dist    == distance start dest@
addVector :: Vector -> Point -> Point
addVector (d,h) p =
                  p { pntLon = toDegrees lon2
                    , pntLat = toDegrees lat2
                        }
  where
        (lat,lon) = getRadianPair p
        lat2 = asin (sin lat * cos (d / radiusOfEarth) + cos lat
                     * sin(d/radiusOfEarth) * cos h)
        lon2 = lon + atan2 (sin h * sin (d / radiusOfEarth) * cos lat)
                           (cos (d/radiusOfEarth) - sin lat * sin lat2)

-- | Speed in meters per second, only if a 'Time' was recorded for each waypoint.
speed :: Point -> Point -> Maybe Speed
speed a b = 
  case (pntTime b, pntTime a) of
    (Just x, Just y) -> 
      let timeDiff = realToFrac (diffUTCTime x y)
      in if timeDiff == 0 then Nothing else Just $ (distance a b) / timeDiff
    _ -> Nothing

-- | Radius of the Earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6378700

-- | Circumference of Earth (meters)
circumferenceOfEarth :: Double
circumferenceOfEarth = radiusOfEarth * 2 * pi

-- | North is 0 radians
north :: Heading
north = 0

-- | South, being 180 degrees from North, is pi.
south :: Heading
south = pi

-- | East is 270 degrees (3 pi / 2)
east :: Heading
east = (3 / 2) * pi

-- | West is 90 degrees (pi / 2)
west :: Heading
west = pi / 2

toDegrees :: Double -> Double
toDegrees = (*) (180 / pi)

-- Get latitude and longitude in Radians as 'Double's
getRadianPair :: Point -> (Double,Double)
getRadianPair p = (toRadians (pntLat p), toRadians (pntLon p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

-- | @interpolate c1 c2 w@ where @0 <= w <= 1@ Gives a point on the line
-- between c1 and c2 equal to c1 when @w == 0@ (weighted linearly
-- toward c2).
interpolate :: Point -> Point -> Double -> Point
interpolate c1 c2 w
  | w < 0 = c1
  | w > 1 = c2
  | otherwise = 
  let (h,d) = (heading c1 c2, distance c1 c2)
      v = (d * w, h)
  in addVector v c1

-- | Compute the points at which two circles intersect (assumes a flat plane).  If
-- the circles do not intersect or are identical then the result is @Nothing@.
circleIntersectionPoints :: (Point, Distance) -> (Point, Distance) -> Maybe (Point,Point)
circleIntersectionPoints (a,r1) (b,r2)
  | pntLat a == pntLat b && pntLon a == pntLon b && r1 == r2 = Nothing -- FIXME need approx eq
  | r1 + r2 < ab = Nothing
  | any isNaN (map pntLat pts) || any isNaN (map pntLon pts) = Nothing
  | otherwise = Just (p1, p2)
  where
  ab = distance a b
  angABX = acos ( (r1^(2::Int) + ab^(2::Int) - r2^(2::Int)) / (2 * r1 * ab) )
  ang1 = heading a b + angABX
  ang2 = heading a b - angABX
  p1 = addVector (r1, ang1) a
  p2 = addVector (r1, ang2) a
  pts = [p1,p2]

-- | Find the area in which all given circles intersect.  The resulting
-- area is described in terms of the bounding arcs.   All circles must
-- intersect at two points.
intersectionArcsOf :: [Circle Point] -> [Arc Point]
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

maximumDistanceOfArc :: Point -> Arc Point -> Distance
maximumDistanceOfArc pnt ((c,r), h1, h2) =
  let pcHeading = heading pnt c
  in if ((pcHeading < h1 || pcHeading > h2) && h1 < h2) || ((pcHeading > h2 && pcHeading < h1) && h1 > h2)
         then max (distance pnt (addVector (r,h1) c)) (distance pnt (addVector (r,h2) c))
         else distance pnt c + r

choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:xs) = map (x,) xs ++ choose2 xs
