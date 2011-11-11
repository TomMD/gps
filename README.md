GPS
=================
#### Querying and manipulating gps coordinates
* * *

This is a Haskell library for querying and manipulating GPS
coordinates and/or trails (lists of coordinates).

CURRENT STATE
------------- 

The basic queries such as speed, distance or heading between points,
adding a vector to a point, average speed for a track, and total track
distance all operate.  Some trivial quickcheck properties are included
but they are nowhere near complete.

Advanced operations include path smoothing (using bezier curve point
interpolation), rest location extraction, and coordinate grouping operations.
