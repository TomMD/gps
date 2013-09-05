{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, EmptyDataDecls, BangPatterns, TupleSections #-}
-- |A basic GPS library with calculations for distance and speed along
-- with helper functions for filtering/smoothing trails.  All distances
-- are in meters and time is in seconds.  Speed is thus meters/second

module Geo.Computations
       ( module Geo.Computations.Basic
       , module Geo.Computations.Trail
       , module Geo.Types
       ) where

import Geo.Computations.Basic
import Geo.Computations.Trail
import Geo.Types
