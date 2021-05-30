{-# LANGUAGE TupleSections #-}
module Geo.Computations.Trail
       ( -- * Types         
         AvgMethod(..)
       , Selected(..)
       , PointGrouping
       , TransformGrouping
         -- * Utility Functions
       , isSelected
       , isNotSelected
       , onSelected
       , selLength
         -- * Trail Functions
         -- ** Queries
       , totalDistance
       , totalTime
       , avgSpeeds
       , slidingAverageSpeed
       , closestDistance
       , convexHull
         -- ** Transformations
       , bezierCurveAt
       , bezierCurve
       , linearTime
       , filterPoints
         -- ** Grouping Methods
       , betweenSpeeds
       , restLocations
       , spansTime
       , everyNPoints
         -- ** Group Transformations 
       , intersectionOf
       , invertSelection
       , firstGrouping
       , lastGrouping
       , unionOf
       , refineGrouping
       , (/\), (\/)
         -- ** Composite Operations (Higher Level)
       , smoothRests
       , smoothTrail
        -- * Misc
       , bezierPoint
         ) where

import Text.Show.Functions ()
import Geo.Computations.Basic

import Control.Arrow (first)
import Control.Monad
import Data.Fixed (mod')
import Data.Function (on)
import Data.List as L
import Data.Maybe
import Data.Ord
import Data.Time


import Statistics.Function as F
import Statistics.Sample
import qualified Data.Vector.Unboxed as V

takeWhileEnd :: (a -> Bool) -> [a] -> (Maybe a, [a],[a])
takeWhileEnd p xs = go xs Nothing
  where
  go [] e = (e, [], [])
  go (a:as) e
    | p a = let (e',xs2,zs) = go as (Just a) in (e',a:xs2,zs)
    | otherwise = (e,[], a:as)

data AvgMethod c
  = AvgMean              -- ^ Obtain the 'mean' of the considered points
  | AvgHarmonicMean      -- ^ Obtain the 'harmonicMean'
  | AvgGeometricMean     -- ^ Obtain the 'geometricMean'
  | AvgMedian            -- ^ Obtain the median of the considered points
  | AvgEndPoints         -- ^ Compute the speed considering only the given endpoints
  | AvgMinOf [AvgMethod c] -- ^ Take the minimum of the speeds from the given methods
  | AvgWith ([c] -> Speed)
    
-- | @avgSpeeds n points@
-- Average speed using a window of up to @n@ seconds and averaging by taking the
-- Median ('AvgMedian').
avgSpeeds :: NominalDiffTime -> Trail Point -> [(UTCTime, Speed)]
avgSpeeds = slidingAverageSpeed AvgHarmonicMean

-- | @slidingAverageSpeed m n@ Average speed using a moving window of up to @n@ seconds
-- and an 'AvgMethod' of @m@.
slidingAverageSpeed :: AvgMethod Point -> NominalDiffTime -> Trail Point -> [(UTCTime, Speed)]
slidingAverageSpeed _ _ [] = []
slidingAverageSpeed m minTime xs =
  let pts   = map unSelect (spansTime minTime xs)
      spds  = map (getAvg m) pts
      times = map getAvgTimes pts
  in concatMap maybeToList $ zipWith (\t s -> fmap (,s) t) times spds
  where
  getTimeDiff a b = on (liftM2 diffUTCTime) pntTime a b
  
  --  getAvg :: [] -> AvgMethod -> Speed
  getAvg _ [] = 0
  getAvg _ [_] = 0
  getAvg m2 cs =
    let ss = getSpeedsV cs
    in case m2 of
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
        AvgEndPoints -> fromMaybe 0 $ speed (head cs) (last cs)
        AvgMinOf as -> minimum $ map (flip getAvg cs) as
        AvgWith f -> f cs
  getAvgTimes [] = Nothing
  getAvgTimes [x] = pntTime x
  getAvgTimes ps = getAvgTime (head ps) (last ps)
  getAvgTime a b = liftM2 addUTCTime (getTimeDiff b a) (pntTime a)
  getSpeedsV = V.fromList . getSpeeds
  getSpeeds zs = concatMap maybeToList $ zipWith speed zs (drop 1 zs)

-- | A PointGrouping is a function that selects segments of a trail.
-- 
-- Grouping point _does not_ result in deleted points. It is always true that:
--
--     forall g :: PointGrouping c -->
--     concatMap unSelect (g ts) == ts
--
-- The purpose of grouping is usually for later processing.  Any desire to drop
-- points that didn't meet a particular grouping criterion can be filled with
-- a composition with 'filter' (or directly via 'filterPoints').
type PointGrouping c = Trail c -> [Selected (Trail c)]

-- | Given a selection of coordinates, transform the selected
-- coordinates in some way (while leaving the non-selected
-- coordinates unaffected).
type TransformGrouping c = [Selected (Trail c)] -> [Selected (Trail c)]

-- | When grouping points, lists of points are either marked as 'Select' or 'NotSelect'.
data Selected a = Select {unSelect :: a} | NotSelect {unSelect :: a}
  deriving (Eq, Ord, Show)

isSelected :: Selected a -> Bool
isSelected (Select _) = True
isSelected _ = False

isNotSelected :: Selected a -> Bool
isNotSelected = not . isSelected

selLength :: Selected [a] -> Int
selLength = length . unSelect

onSelected :: (a -> b) -> (a -> b) -> Selected a -> b
onSelected f _ (Select a) = f a
onSelected _ g (NotSelect a) = g a

instance Functor Selected where
  fmap f (Select x) = Select $ f x
  fmap f (NotSelect x) = NotSelect $ f x

dropExact :: Int -> [Selected [a]] -> [Selected [a]]
dropExact _ [] = []
dropExact i (x:xs) =
  case compare (selLength x) i of
    EQ -> xs
    LT -> dropExact (i - selLength x) xs
    GT -> fmap (drop i) x : xs

-- | Groups trail segments into contiguous points within the speed
-- and all others outside of the speed.  The "speed" from point p(i)
-- to p(i+1) is associated with p(i) (except for the first speed
-- value, which is associated with both the first and second point)
betweenSpeeds :: Double -> Double -> PointGrouping Point
betweenSpeeds low hi ps =
  let spds = concatMap maybeToList $ zipWith speed ps (drop 1 ps)
      psSpds = [(p,s) | p <- ps, s <- maybeToList (listToMaybe spds) ++ spds]
      inRange x = x >= low && x <= hi
      chunk [] = []
      chunk xs@(x:_) =
        let op p = if inRange (snd x) then first Select . span p else first NotSelect . break p
            (r,rest) = op (inRange . snd) xs
        in r : chunk rest
  in map (fmap (map fst)) $ chunk psSpds

-- | A "rest point" means the coordinates remain within a given distance
-- for at least a particular amount of time.
restLocations :: Distance -> NominalDiffTime -> PointGrouping Point
restLocations d s ps =
  let consToFirst x [] = [NotSelect [x]]
      consToFirst x (a:as) = (fmap (x:) a) : as
      go [] [] = []
      go [] nonRests = [NotSelect $ reverse nonRests]
      go (a:as) nonRests =
        case takeWhileEnd ((<=) d . distance a) as of
          (Just l, close, far) ->
            case (pntTime a, pntTime l) of
              (Just t1, Just t2) ->
                let diff = diffUTCTime t2 t1
                in if diff >= s then NotSelect (reverse nonRests) : Select (a:close) : go far [] else go as (a:nonRests)
              _ -> consToFirst a $ go as nonRests
          _ -> consToFirst a $ go as nonRests
  in go ps []
     
-- | Chunks points into groups spanning at most the given time
-- interval.
spansTime :: NominalDiffTime -> PointGrouping Point
spansTime n ps =
  let times  = mkTimePair ps
      chunk [] = []
      chunk xs@(x:_) =
        let (good,rest) = span ((<= addUTCTime n (snd x)) . snd) xs 
        in if null good then [xs] else good : chunk rest
  in map (Select . map fst) $ chunk times

-- | Intersects the given groupings
intersectionOf :: [PointGrouping Point] -> PointGrouping Point
intersectionOf gs ps =
  let groupings = map ($ ps) gs
      -- chunk :: [[Selected [pnts]]] -> pnts -> [pnts]
      chunk _ [] = []
      chunk ggs xs = 
        let minLen = max 1 . minimum . concatMap (take 1) $ map (map selLength) ggs   -- FIXME this is all manner of broken
            sel = if all isSelected (concatMap (take 1) ggs) then Select else NotSelect
            (c,rest) = splitAt minLen xs
        in sel c : chunk (filter (not . null) $ map (dropExact minLen) ggs) rest
  in chunk groupings ps

-- | Union all the groupings
unionOf :: [PointGrouping Point] -> PointGrouping Point
unionOf gs ps =
  let groupings = map ($ ps) gs
      chunk _ [] = []
      chunk ggs xs =
        let getSegs = concatMap (take 1)
            segs = getSegs ggs
            len =
              if any isSelected segs
                 then max 1 . maximum . getSegs . map (map selLength) . map (filter isSelected) $ ggs
                 else max 1 . minimum . getSegs . map (map selLength) $ ggs
            sel = if any isSelected segs then Select else NotSelect
            (c,rest) = splitAt len xs
        in sel c : chunk (filter (not . null) $ map (dropExact len) ggs) rest
  in chunk groupings ps
     
-- | Intersection binary operator
(/\) :: [Selected (Trail a)] -> TransformGrouping a
(/\) _ [] = []
(/\) [] _ = []
(/\) xsL@(Select x:_) ysL@(Select y:_) =
  let z = if length x < length y then x else y
      xs' = selListDrop (length z) xsL
      ys' = selListDrop (length z) ysL
  in Select z : (xs' /\ ys')
(/\) xs (NotSelect y:ys) = NotSelect y : (selListDrop (length y) xs /\ ys)
(/\) (NotSelect x:xs) ys = NotSelect x : (xs /\ selListDrop (length x) ys)

-- | Union binary operator
(\/) :: [Selected (Trail a)] -> TransformGrouping a
(\/) xs [] = xs
(\/) [] ys = ys
(\/) (Select x:xs) (Select y : ys) =
  let xLen = length x
      yLen = length y
  in if xLen < yLen
       then (Select y :) (selListDrop (yLen - xLen) xs \/ ys)
       else (Select x :) (xs \/ selListDrop (xLen - yLen) ys)
(\/) (Select x:_) ys = Select x : selListDrop (length x) ys
(\/) xs (Select y:_) = Select y : selListDrop (length y) xs
(\/) xsL@(NotSelect x:xs) ysL@(NotSelect y:ys) =
  let xLen = length x
      yLen = length y
  in if xLen < yLen
        then (NotSelect x:) (xs \/ selListDrop xLen ysL)
        else (NotSelect y:) (selListDrop yLen xsL \/ ys)

selListDrop :: Int -> [Selected [a]] -> [Selected [a]]
selListDrop 0 xs = xs
selListDrop _ [] = []
selListDrop n (x:xs) =
  let x' = drop n (unSelect x)
  in fmap (const x') x : selListDrop (n - (selLength x - length x')) xs

-- | Inverts the selected/nonselected segments
invertSelection :: TransformGrouping a
invertSelection = map (onSelected NotSelect Select)

-- | @firstGrouping f ps@ only the first segment remains 'Select'ed, and only
-- if it was already selected by @f@.
firstGrouping ::  TransformGrouping a
firstGrouping ps = take 1 ps ++ map (NotSelect . unSelect) (drop 1 ps)

-- | Only the last segment, if any, is selected (note: the current
-- implementation is inefficient, using 'reverse')
lastGrouping ::  TransformGrouping a
lastGrouping ps  = let ps' = reverse ps in reverse $ take 1 ps' ++ map (NotSelect . unSelect) (drop 1 ps')

-- | Chunks the trail into groups of N points
everyNPoints ::  Int -> PointGrouping a
everyNPoints n ps
  | n <= 0 = [NotSelect ps]
  | otherwise = go ps
    where
      go [] = []
      go xs = let (h,t) = splitAt n xs in Select h : go t
  
-- | For every selected group, refine the selection using the second
-- grouping method.  This differs from 'IntersectionOf' by restarting
-- the second grouping algorithm at the beginning each group selected
-- by the first algorithm.
refineGrouping ::  PointGrouping a -> TransformGrouping a
refineGrouping b = concatMap (onSelected b (\x -> [NotSelect x]))

-- | Remove all points that remain 'NotSelect'ed by the given grouping algorithm.
filterPoints :: PointGrouping a -> Trail a -> Trail a
filterPoints g = concatMap unSelect . filter isSelected . g

-- Extract the time from each coordinate.  If no time is available then
-- the coordinate is dropped!
mkTimePair :: Trail Point -> [(Point,UTCTime)]
mkTimePair xs =
  let timesM = map (\x-> fmap (x,) $ pntTime x) xs
  in concatMap maybeToList timesM

-- | Construct a bezier curve using the provided trail.  Construct a
-- new trail by sampling the given bezier curve at the given times.
-- The current implementation assumes the times of the input
-- coordinates are available and all equal (Ex: all points are 5
-- seconds apart), the results will be poor if this is not the case!
bezierCurveAt :: [UTCTime] -> Trail Point -> Trail Point
bezierCurveAt _ [] = []
bezierCurveAt selectedTimes xs = 
  let timesDef = mkTimePair xs
      end = last timesDef
      top = head timesDef
      tTime  = diffUTCTime (snd end) (snd top)
      times = if null selectedTimes then map snd timesDef else selectedTimes
      diffTimes = [diffUTCTime t (snd top) / tTime | t <- times]
      queryTimes = map realToFrac diffTimes
  in if tTime <= 0 || any (\x -> x < 0 || x > 1) queryTimes
        then xs -- error "bezierCurveAt has a out-of-bound time!"
        else
         if null timesDef || any (\x -> x < 0 || x > 1) queryTimes
         then xs
         else let curvePoints = (map (bezierPoint xs) queryTimes)
                  newTimes = [addUTCTime t (snd top) | t <- diffTimes]
              in zipWith (\t p -> p { pntTime = Just t}) newTimes curvePoints

bezierPoint :: [Point] -> Double -> Point
bezierPoint pnts t   = go pnts
  where
  go [] = error "GPS Package: Can not create a bezier point from an empty list"
  go [p] = p
  go ps = interpolate (go (init ps)) (go (tail ps)) t

-- | Interpolate selected points onto a bezier curve.  Note this gets
-- exponentially more expensive with the length of the segment being
-- transformed - it is not advisable to perform this operation on
-- trail segements with more than ten points!
bezierCurve ::  [Selected (Trail Point)] -> Trail Point
bezierCurve = concatMap (onSelected (bezierCurveAt []) Prelude.id)

-- | Filter out any points that go backward in time (thus must not be
-- valid if this is a trail)
linearTime :: [Point] -> [Point]
linearTime [] = []
linearTime (p:ps) = go (pntTime p) ps
  where
  go _ [] = []
  go t (x:xs) = if pntTime x < t then go t xs else x : go (pntTime x) xs

-- | Return the closest distance between two trails (or Nothing if a
-- trail is empty).  Inefficient implementation:
-- O( (n * m) * log (n * m) )
closestDistance :: Trail Point -> Trail Point -> Maybe Distance
closestDistance as bs = listToMaybe $ L.sort [distance a b | a <- as, b <- bs]

-- | Find the total distance traveled
totalDistance :: [Point] -> Distance
totalDistance as = sum $ zipWith distance as (drop 1 as)

totalTime :: Trail Point -> NominalDiffTime
totalTime [] = 0
totalTime xs@(x:_) = fromMaybe 0 $ liftM2 diffUTCTime (pntTime $ last xs) (pntTime x)

-- | Uses Grahams scan to compute the convex hull of the given points.
-- This operation requires sorting of the points, so don't try it unless
-- you have notably more memory than the list of points will consume.
convexHull :: [Point] -> [Point]
convexHull lst =
        let frst = southMost lst
        in case frst of
                Nothing -> []
                Just f  ->
                     let sorted = L.sortBy (comparing (eastZeroHeading f)) (filter (/= f) lst)
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
                _         -> grahamScan (p1:ps) (x:xs)

eastZeroHeading :: Point -> Point -> Heading
eastZeroHeading s = (`mod'` (2*pi)) . (+ pi/2) . heading s

data Turn = LeftTurn | RightTurn | Straight deriving (Eq, Ord, Show, Read, Enum)

turn :: Point -> Point -> Point -> Turn
turn a b c =
        let h1 = eastZeroHeading a b
            h2 = eastZeroHeading b c
            d  = h2 - h1
        in if d >= 0 && d < pi then LeftTurn else RightTurn

-- | Find the southmost point
southMost :: [Point] -> Maybe Point
southMost []  = Nothing
southMost cs = Just . minimumBy (comparing pntLat) $ cs

---------- COMPOSIT OPERATIONS ---------------
-- These operations are simply implemented using the previously
-- defined functions. They can serve either for concise use for novice
-- users or as instructional examples.
------------------------------------------

-- | Smooth points with rest areas using a bezierCurve.
--
-- Parameters: rest for 1 minute within 30 meters get smoothed
-- in a bezier curve over every 8 points.
smoothRests :: Trail Point -> Trail Point
smoothRests = bezierCurve . refineGrouping (everyNPoints 8) . restLocations 30 60

-- | Smooth every 7 points using a bezier curve
smoothTrail :: Trail Point -> Trail Point
smoothTrail = gSmoothSome 7

-- | Smooth every n points using a bezier curve
gSmoothSome :: Int -> Trail Point -> Trail Point
gSmoothSome n = bezierCurve . everyNPoints n
