# Coerce Data Into a sealevel Object

Coerces a dataset (minimally, a sequence of times and heights) into a
sealevel dataset. The arguments are based on the standard data format,
as were described in a file formerly available at reference 1.

## Usage

``` r
as.sealevel(
  elevation,
  time,
  header = NULL,
  stationNumber = NA,
  stationVersion = NA,
  stationName = NULL,
  region = NULL,
  year = NA,
  longitude = NA,
  latitude = NA,
  GMTOffset = NA,
  decimationMethod = NA,
  referenceOffset = NA,
  referenceCode = NA,
  deltat
)
```

## Arguments

- elevation:

  a list of sea-level heights in metres, in an hourly sequence.

- time:

  optional list of times, in POSIXct format. If missing, the list will
  be constructed assuming hourly samples, starting at 0000-01-01
  00:00:00.

- header:

  a character string as read from first line of a standard data file.

- stationNumber:

  three-character string giving station number.

- stationVersion:

  single character for version of station.

- stationName:

  the name of station (at most 18 characters).

- region:

  the name of the region or country of station (at most 19 characters).

- year:

  the year of observation.

- longitude:

  the longitude in decimal degrees, positive east of Greenwich.

- latitude:

  the latitude in decimal degrees, positive north of the equator.

- GMTOffset:

  offset from GMT, in hours.

- decimationMethod:

  a coded value, with 1 meaning filtered, 2 meaning a simple average of
  all samples, 3 meaning spot readings, and 4 meaning some other method.

- referenceOffset:

  ?

- referenceCode:

  ?

- deltat:

  optional interval between samples, in hours (as for the
  [`ts()`](https://rdrr.io/r/stats/ts.html) timeseries function). If
  this is not provided, and `t` can be understood as a time, then the
  difference between the first two times is used. If this is not
  provided, and `t` cannot be understood as a time, then 1 hour is
  assumed.

## Value

A
[sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
object (for details, see
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md)).

## References

`http://ilikai.soest.hawaii.edu/rqds/hourly.fmt` (this link worked for
years but failed at least temporarily on December 4, 2016).

## See also

The documentation for the
[sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
class explains the structure of sealevel objects, and also outlines the
other functions dealing with them.

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`read.sealevel.gc2026()`](https://dankelley.github.io/oce/reference/read.sealevel.gc2026.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)

# Construct a year of M2 tide, starting at the default time
# 0000-01-01T00:00:00.
h <- seq(0, 24 * 365)
elevation <- 2.0 * sin(2 * pi * h / 12.4172)
sl <- as.sealevel(elevation)
summary(sl)
#> Sealevel Summary
#> ----------------
#> 
#> * sampling delta-t:    1 hour
#> * Location:            Lat and lon unknown 
#> * number of observations:   8761 
#> *    "      non-missing:    8761 
#> * Time: 0000-01-01 to 0000-12-31 (8761 samples, mean increment 1 hour)
#> * Data Overview
#> 
#>                   Min.        Mean        Max.        Dim. NAs
#>     elevation [m] -2          0.00089588  2           8761 0  
#>     time          -6.2167e+10 -6.2151e+10 -6.2136e+10 8761 0  
#> 
#> * Processing Log
#> 
#>     - 2026-03-16 12:25:41 UTC: `create 'sealevel' object`
#>     - 2026-03-16 12:25:41 UTC: `as.sealevel(elevation = elevation)`

# As above, but start at the Y2K time.
time <- as.POSIXct("2000-01-01") + h * 3600
sl <- as.sealevel(elevation, time)
summary(sl)
#> Sealevel Summary
#> ----------------
#> 
#> * sampling delta-t:    1 hour
#> * Location:            Lat and lon unknown 
#> * number of observations:   8761 
#> *    "      non-missing:    8761 
#> * Time: 2000-01-01 04:00:00 to 2000-12-31 04:00:00 (8761 samples, mean increment 1 hour)
#> * Data Overview
#> 
#>                   Min.      Mean       Max.      Dim. NAs
#>     elevation [m] -2        0.00089588 2         8761 0  
#>     time          946699200 962467200  978235200 8761 0  
#> 
#> * Processing Log
#> 
#>     - 2026-03-16 12:25:41 UTC: `create 'sealevel' object`
#>     - 2026-03-16 12:25:41 UTC: `as.sealevel(elevation = elevation, time = time)`
```
