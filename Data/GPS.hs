{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, EmptyDataDecls, BangPatterns, TupleSections #-}
-- |A basic GPS library with calculations for distance and speed along
-- with helper functions for filtering/smoothing trails.  All distances
-- are in meters and time is in seconds.  Speed is thus meters/second

module Data.GPS
       ( module Data.GPS.Core
       , module Data.GPS.Trail
       ) where

import Data.GPS.Core
import Data.GPS.Trail