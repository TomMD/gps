module Geo.Types where
import Data.Text
import Data.Time

data Track = Track
        { trkName               :: Maybe Text
        , trkDescription        :: Maybe Text
        , segments              :: [Segment]
        }
        deriving (Eq, Ord, Show, Read)

-- |A GPX segments is just a bundle of points.
data Segment = Segment { points  :: [Point] }
        deriving (Eq, Ord, Show, Read)

type Latitude = Double
type Longitude = Double

-- |Track point is a full-fledged representation of all the data
-- available in most GPS loggers.  It is possible you don't want
-- all this data and can just made do with coordinates (via 'Pnt')
-- or a custom derivative.
data Point = Point
        { pntLat        :: Latitude
        , pntLon        :: Longitude
        , pntEle        :: Maybe Double -- ^ In meters
        , pntTime       :: Maybe UTCTime
        -- , pntSpeed   :: Maybe Double -- ^ Non-standard.  Usually in meters/second.
        }
        deriving (Eq, Ord, Show, Read)

pt :: Latitude -> Longitude -> Maybe Double -> Maybe UTCTime -> Point
pt t g e m = Point t g e m

zeroPoint = Point 0 0 Nothing Nothing
