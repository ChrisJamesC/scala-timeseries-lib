# tslib -- a lightweight time series library

## Why 
I've had to handle time-series like data in Java recently, which turned out to be ~~slightly~~ really frustrating.

Having some spare time and wanting to see what I could come up with in Scala, I decided to build a small time series library. Additional reasons are:

  - It's fun
  - There seems to be no library doing something like that out there
  - I wanted to write some Scala again.

## TODOS
  - updatable timeseries (ala immutable collection, or mutable collection style)
  - compression (at least for strict equality)
  - slicing
  - check if sorting of sequences for merges is required/efficient
  - decent tests for non-trivial merge operators
  - default Seq implementation (and the one that is imported) is mutable -> consider the implications and see if we can easily fix this by 'import scala.collection.immutable.Seq' everywhere required.
  - input validation when applying. Check entries sorted (for the vector TS) and without overlap.
  - Have empty time series always be represented by an EmptyTimeSeries. (Ie, wrapping an empty vector or map with a vector/treemap time-series should not happen)
  - Generic tests for any kind of TS implementation
