import Data.GPS
import Data.Time
import Data.List
import Data.Ord
import Data.Fixed
import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Text.XML.XSD.DateTime
import Control.Applicative
import Control.Monad

instance Arbitrary LatitudeType where
  arbitrary = liftM (latitudeType . flip mod' 180) arbitrary

instance Arbitrary LongitudeType where
  arbitrary = liftM (longitudeType . flip mod' 180) arbitrary

instance Arbitrary DateTime where
  arbitrary = liftM fromUTCTime arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> liftM (secondsToDiffTime . abs) (arbitrary :: Gen Integer)

instance Arbitrary Day where
  arbitrary = fromGregorian <$> liftM ((+ 1000) . (`mod` 1100) .abs) arbitrary <*> liftM abs arbitrary <*> liftM abs arbitrary

instance Arbitrary NominalDiffTime where
  arbitrary = liftM fromIntegral (arbitrary :: Gen Int)
instance Arbitrary WptType where
  arbitrary =
    wptType <$> arbitrary -- Lat
            <*> arbitrary -- Lon
            <*> arbitrary -- Time
            <*> arbitrary -- elevation
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return []           -- LinkType
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing
            <*> return Nothing

newtype Trl = Trl [WptType]
  deriving (Show)

instance Arbitrary Trl where
  arbitrary = do
    b <- arbitrary :: Gen [Int]
    u_ts_d <- mapM (\i -> (,,) <$> arbitrary <*> replicateM i arbitrary <*> arbitrary) b :: Gen [(UTCTime, [WptType],NominalDiffTime)]
    let u_ts_d' = sortBy (comparing (\(a,_,_) -> a)) u_ts_d
        xs = concat [zipWith (setTime' . fromUTCTime) (iterate (addUTCTime d) u) x | (u,x,d) <- u_ts_d']
    return $ Trl xs

approxEq :: WptType -> WptType -> Bool
approxEq a b = distance a b <= 0.2 -- error of 13cm has been observed due to floating point issues when using add vector.

pSaneDistance :: WptType -> WptType -> Bool
pSaneDistance a b = distance a b <= circumferenceOfEarth / 2

pTriangleTheorem :: WptType -> WptType -> WptType -> Bool
pTriangleTheorem a b c = 
    distance a b + distance b c >= distance a c  -- Traditional flat-surface geometry
 || distance a b + distance b c + distance c a == 2 * pi * radiusOfEarth

pAddVector_DistanceHeading_ident :: WptType -> WptType -> Bool
pAddVector_DistanceHeading_ident a b =
  let v = (distance a b, heading a b)
      c = addVector v a
  in (distance c b) <= 0.01 * (distance a b)

pConvexHull_Has_Extreme_Points :: Trl -> Bool
pConvexHull_Has_Extreme_Points (Trl ts) =
  let ch = convexHull ts
      ts' = sortBy (comparing lat) ts
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
  , testGroup "Trail Computations"
    [ testProperty "Hull has extreme points" pConvexHull_Has_Extreme_Points
    , testProperty "HullContainsBezier" pConvexHull_Bezier_Const]
  ]


----------------------------------------------------------
------------- * HARNESS * --------------------------------
----------------------------------------------------------
main :: IO ()
main = defaultMain tests
