{-# LANGUAGE TupleSections #-}
module Data.GPS.Trail
       ( -- * Types         
         AvgMethod(..)
       , Selected(..)
         -- * Utility Functions
       , isSelected
       , isNotSelected
       , onSelected
       , selLength
         -- * Trail Functions
         -- ** Queries
       , totalDistance
       , avgSpeeds
       , slidingAverageSpeed
       , closestDistance
       , convexHull
         -- ** Transformations
       , bezierCurve
       , linearTime
       , filterPoints
         -- ** Grouping Methods
       , betweenSpeeds
       , restLocations
       , spansTime
       , everyNPoints
       , intersectionOf
       , invertSelection
       , firstGrouping
       , lastGrouping
       , unionOf
       , refineGrouping
         -- ** Composite Operations (Higher Level)
       , smoothStoppedPoints
         ) where

import Text.Show.Functions ()
import Data.GPS.Core hiding (fix)

import Control.Arrow (first, second)
import Control.Monad
import Data.Fixed (mod')
import Data.Function (on,fix)
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
    | p a = let (e',xs,zs) = go as (Just a) in (e',a:xs,zs)
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
avgSpeeds :: (Lat a, Lon a, Time a) => NominalDiffTime -> Trail a -> [(UTCTime, Speed)]
avgSpeeds = slidingAverageSpeed AvgHarmonicMean

-- | @slidingAverageSpeed m n@ Average speed using a moving window of up to @n@ seconds
-- and an 'AvgMethod' of @m@.
slidingAverageSpeed :: (Lat a, Lon a, Time a) => 
                       AvgMethod a -> NominalDiffTime -> Trail a -> [(UTCTime, Speed)]
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
        AvgWith f -> f cs
  getAvgTime a b = liftM2 addUTCTime (getTimeDiff b a) (getUTCTime a)
  getSpeedsV = V.fromList . getSpeeds
  getSpeeds zs = concatMap (maybeToList . uncurry speed) $ zip zs (drop 1 zs)

type TrailTransformation c = [Selected (Trail c)] -> Trail c

-- | A PointGrouping is a function that selects segments of a trail.
-- 
-- Grouping point _does not_ result in deleted points. It is always true that:
--
--     forall g : PointGrouping c -->
--     concatMap unSelect (g ts) == ts
--
-- The purpose of grouping is usually for later processing.  Any desire to drop
-- points that didn't meet a particular grouping criteria can be filled with
-- a composition with 'filter' (or directly via 'filterPoints'.
type PointGrouping c = Trail c -> [Selected (Trail c)]

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
dropExact i [] = []
dropExact i (x:xs) =
  case compare (selLength x) i of
    EQ -> xs
    LT -> dropExact (i - selLength x) xs
    GT -> fmap (drop i) x : xs

-- | Groups trail segments into contiguous points within the speed
-- and all others outside of the speed.  The "speed" from point p(i)
-- to p(i+1) is associated with p(i) (execpt for the first speed
-- value, which is associated with both the first and second point)
betweenSpeeds :: (Lat a, Lon a, Time a) => Double -> Double -> PointGrouping a
betweenSpeeds low hi ps =
  let spds = concatMap maybeToList $ zipWith speed ps (drop 1 ps)
      psSpds = [(p,s) | p <- ps, s <- maybeToList (listToMaybe spds) ++ spds]
      inRange x = x >= low && x <= hi
      chunk [] = []
      chunk xs@(x:_) =
        let op p = if inRange (snd x) then first Select . span p else first NotSelect . break p
            (r,rest) = op (inRange . snd) xs
        in r : chunk xs
  in map (fmap (map fst)) $ chunk psSpds

-- | A "rest point" means the coordinates remain within a given distance
-- for at least a particular amount of time.
restLocations :: (Lat a, Lon a, Time a) => Distance -> NominalDiffTime -> PointGrouping a
restLocations d s ps =
  let consToFirst x [] = [NotSelect [x]]
      consToFirst x (a:as) = (fmap (x:) a) : as
      go [] [] = []
      go [] nonRests = [NotSelect $ reverse nonRests]
      go (a:as) nonRests =
        case takeWhileEnd ((<=) d . distance a) as of
          (Just l, close, far) ->
            case (getUTCTime a, getUTCTime l) of
              (Just t1, Just t2) ->
                let diff = diffUTCTime t2 t1
                in if diff >= s then NotSelect (reverse nonRests) : Select (a:close) : go far [] else go as (a:nonRests)
              _ -> consToFirst a $ go as nonRests
          _ -> consToFirst a $ go as nonRests
  in go ps []
     
-- |chunking points into groups spanning at most the given time
-- interval.
spansTime :: (Lat a, Lon a, Time a) => NominalDiffTime -> PointGrouping a
spansTime n ps =
  let times  = mkTimePair ps
      chunk [] = []
      chunk (x:xs) =
        let (good,rest) = span ((<= addUTCTime n (snd x)) . snd) xs in good : chunk rest
  in map (Select . map fst) $ chunk times

-- | intersects the given groupings
intersectionOf :: (Lat a, Lon a, Time a) => [PointGrouping a] -> PointGrouping a
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
unionOf :: (Lat a, Lon a, Time a) => [PointGrouping a] -> PointGrouping a
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
(/\) :: Selected (Trail a) -> TransformGrouping a
(/\) a b = undefined

-- | Union binary operator
(\/) :: Selected (Trail a) -> TransformGrouping a
(\/) a b = undefined

-- |Inverts the selected/nonselected segments
invertSelection :: (Lat a, Lon a, Time a) => TransformGrouping a
invertSelection = map (onSelected NotSelect Select)

-- |@firstGrouping f ps@ only the first segment remains 'Select'ed, and only
-- if it was already selected by @f@.
firstGrouping ::  (Lat a, Lon a, Time a) => TransformGrouping a
firstGrouping ps = take 1 ps ++ map (NotSelect . unSelect) (drop 1 ps)

-- | Only the last segment, if any, is selected (note: the current
-- implementation is inefficient, using 'reverse')
lastGrouping ::  TransformGrouping a
lastGrouping ps  = let ps' = reverse ps in reverse $ take 1 ps' ++ map (NotSelect . unSelect) (drop 1 ps')

-- | chunk the trail into groups of N points
everyNPoints ::  (Lat a, Lon a, Time a) => Int -> PointGrouping a
everyNPoints n ps
  | n <= 0 = [NotSelect ps]
  | otherwise =
  (fix (\k xs -> if null xs then [Select xs] else let (f,s) = splitAt n xs in Select f : k s)) ps
  
-- |For every selected group, refine the selection using the second
-- grouping method.  This differs from 'IntersectionOf' by restarting
-- the second grouping algorithm at the beginning each group selected
-- by the first algorithm.
refineGrouping ::  (Lat a, Lon a, Time a) => PointGrouping a -> TransformGrouping a
refineGrouping b = concatMap (onSelected b (\x -> [NotSelect x]))

-- |Remove all points that remain 'NotSelect'ed by the given grouping algorithm.
filterPoints :: (Lat a, Lon a, Time a) => PointGrouping a -> Trail a -> Trail a
filterPoints g = concatMap unSelect . filter isSelected . g

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

bezierPoint :: (Lat a, Lon a) => [a] -> Double -> a
bezierPoint []   _ = error "Can not create a bezier point from an empty list"
bezierPoint [p0] _ = p0
bezierPoint ps t   = interpolate (bezierPoint (init ps) t) (bezierPoint (tail ps) t) t

-- |Interpolate selected points onto a bezier curve.  Note this gets
-- exponentially more expensive with the length of the segement being
-- transformed - it is not advisable to perform this operation on
-- trail segements with more than ten points!
bezierCurve ::  (Lat a, Lon a, Time a) => [Selected (Trail a)] -> Trail a
bezierCurve = concatMap (onSelected transformToBezierCurve Prelude.id)

-- |Filters out any points that go backward in time (thus must not be
-- valid if this is a trail)
linearTime :: (Lon a, Lat a, Time a) => [a] -> [a]
linearTime [] = []
linearTime (p:ps) = go (getUTCTime p) ps
  where
  go _ [] = []
  go t (p:ps) = if getUTCTime p < t then go t ps else p : go (getUTCTime p) ps

-- |Returns the closest distance between two trails (or Nothing if a
-- trail is empty).  Inefficient implementation:
-- O( (n * m) * log (n * m) )
closestDistance :: (Lat a, Lon a) => Trail a -> Trail a -> Maybe Distance
closestDistance as bs = listToMaybe $ L.sort [distance a b | a <- as, b <- bs]

-- | Find the total distance traveled
totalDistance :: (Lat a, Lon a) => [a] -> Distance
totalDistance as = sum $ zipWith distance as (drop 1 as)

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

---------- COMPOSIT OPERATIONS ---------------
-- These operations are simply implemented using the previously
-- defined functions. They can serve either for concise use for novice
-- users or as instructional examples.
------------------------------------------

smoothStoppedPoints :: (Lat a, Lon a, Time a) => Trail a -> Trail a
smoothStoppedPoints = bezierCurve . refineGrouping (everyNPoints 8) . restLocations 30 60