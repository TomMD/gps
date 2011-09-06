GPS
=================
#### Querying and manipulating gps coordinates
* * *

This is a Haskell library for querying and manipulating GPS
coordinates and/or trails (lists of coordinates).

CURRENT STATE ------------- 

The basic queries such as speed, distance or heading between points,
adding a vector to a point, average speed for a track, and total track
distance all operate.  Some trivial quickcheck properties are included
but they are nowhere near complete.

THE FUTURE
----------

I plan to eventually add advanced operations such as bezier curve
interpolation of points, high level smoothing of paths to eliminate
GPS error, and classification of modes of travel for tracks.

Unfortunately, the future plans must wait until bugs are eliminated in
many other packages (hxt-extras and GPX are holding us back at GHC <
7.2 and an xsd parse error prevents proper testing of bezier curve
interpolation).
