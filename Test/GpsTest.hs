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
  arbitrary = UTCTime <$> arbitrary <*> liftM fromIntegral (arbitrary :: Gen Integer)

instance Arbitrary Day where
  arbitrary = fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary

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

pCentralAngleDoesn'tCrash :: WptType -> WptType -> Bool
pCentralAngleDoesn'tCrash a b = let x = centralAngle a b in x > 1 || x <= 1 || x < 10

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

pConvexHull_BezierCurve_Const :: Trl -> Int -> Bool
pConvexHull_BezierCurve_Const (Trl ts) n =
  let ch = convexHull ts
      n' = (n `rem` 9) + 1
  in ch == convexHull (ch ++ bezierCurve (everyNPoints n' ts))

tests :: [Test]
tests =
  [
    testGroup "Coordinate Computations"
    [ testProperty "approxEq_id" (\x -> approxEq x x)
    , testProperty "saneCentralAngle" pCentralAngleDoesn'tCrash
    , testProperty "saneDistance" pSaneDistance
    , testProperty "TriangleTheorem" pTriangleTheorem
    , testProperty "Vector identity" pAddVector_DistanceHeading_ident
    ]
  , testGroup "Trail Computations"
    [ testProperty "HullContainsBezier" pConvexHull_BezierCurve_Const]
  ]


----------------------------------------------------------
------------- * HARNESS * --------------------------------
----------------------------------------------------------
main :: IO ()
main = defaultMain tests
