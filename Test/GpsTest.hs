import Geo.Computations
import Data.Time
import Data.List
import Data.Ord
import Data.Fixed
import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Applicative
import Control.Monad

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> liftM (secondsToDiffTime . abs) (arbitrary :: Gen Integer)

instance Arbitrary Day where
  arbitrary = fromGregorian <$> liftM ((+ 1000) . (`mod` 1100) .abs) arbitrary <*> liftM abs arbitrary <*> liftM abs arbitrary

instance Arbitrary NominalDiffTime where
  arbitrary = liftM fromIntegral (arbitrary :: Gen Int)

instance Arbitrary Point where
  arbitrary =
    pt      <$> fmap (`mod'` 90) arbitrary -- Lat
            <*> fmap (`mod'` 180) arbitrary -- Lon
            <*> arbitrary -- Time
            <*> arbitrary -- elevation

newtype Trl = Trl [Point]
  deriving (Show)

instance Arbitrary Trl where
  arbitrary = do
    b <- (`mod` 5) `fmap` arbitrary :: Gen Int
    pnts <- mapM (\_ -> arbitrary) [0..abs b]
    return $ Trl (sortBy (comparing pntTime) pnts)

approxEq :: Point -> Point -> Bool
approxEq a b = distance a b <= 0.2 -- error of 13cm has been observed due to floating point issues when using add vector.

pSaneDistance :: Point -> Point -> Bool
pSaneDistance a b = distance a b <= circumferenceOfEarth / 2

pTriangleTheorem :: Point -> Point -> Point -> Bool
pTriangleTheorem a b c = 
    distance a b + distance b c >= distance a c  -- Traditional flat-surface geometry
 || distance a b + distance b c + distance c a == 2 * pi * radiusOfEarth

pAddVector_DistanceHeading_ident :: Point -> Point -> Bool
pAddVector_DistanceHeading_ident a b =
  let v = (distance a b, heading a b)
      c = addVector v a
  in (distance c b) <= 0.01 * (distance a b)

pConvexHull_Has_Extreme_Points :: Trl -> Bool
pConvexHull_Has_Extreme_Points (Trl ts) =
  let ch = convexHull ts
      ts' = sortBy (comparing pntLat) ts
      northMost = last ts'
      southMost = head ts'
  in length ts < 3 || (northMost `elem` ch && southMost `elem` ch)

pConvexHull_Bezier_Const :: Trl -> Double -> Bool
pConvexHull_Bezier_Const (Trl ts) n =
  let ts' = take 10 ts
      ch  = convexHull ts'
      n'  = abs (n `mod'` 1)
      bp  = bezierPoint ts' n'
      ch' = convexHull  (bp:ts')
  in length ts < 3 || ch == ch'

tests :: [Test]
tests =
  [
    testGroup "Coordinate Computations"
    [ testProperty "approxEq_id" (\x -> approxEq x x)
    , testProperty "saneDistance" pSaneDistance
    , testProperty "TriangleTheorem" pTriangleTheorem
    , testProperty "Vector identity" pAddVector_DistanceHeading_ident
    ]
-- These might make some sense in a local scope, but in a global range what
-- is the "left most" point?  On a sphere what is a convex hull?
--  , testGroup "Trail Computations"
--    [ testProperty "Hull has extreme points" pConvexHull_Has_Extreme_Points
--    , testProperty "HullContainsBezier" pConvexHull_Bezier_Const]
  ]


----------------------------------------------------------
------------- * HARNESS * --------------------------------
----------------------------------------------------------
main :: IO ()
main = defaultMain tests
