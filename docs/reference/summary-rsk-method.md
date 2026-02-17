# Summarize a rsk Object

Summarizes some of the data in a
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) object,
presenting such information as the station name, sampling location, data
ranges, etc.

## Usage

``` r
# S4 method for class 'rsk'
summary(object, ...)
```

## Arguments

- object:

  An [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
  object.

- ...:

  Further arguments passed to or from other methods.

## See also

The documentation for
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) explains
the structure of CTD objects, and also outlines the other functions
dealing with them.

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(rsk)
summary(rsk)
#> rsk summary
#> -----------
#> * Instrument:    model RBRconcerto, serial number 60130
#> * Atm. press.:   10.1325
#> * Pressure type: absolute
#> * Source:        `060130_20150904_1159.rsk`
#> * Time: 2015-09-04 15:37:21 to 2015-09-04 15:44:39 (2633 samples, mean increment 0.1666664 s)
#> * Data Overview
#> 
#>                               Min.        Mean        Max.        Dim. NAs OriginalName
#>     time                      1441381041  1441381260  1441381480  2633 0   "-"         
#>     conductivity [mS/cm]      28.86       30.055      30.985      2633 0   "cond06"    
#>     temperature [°C, ITS-90]  0.92965     1.9055      4.3835      2633 0   "temp03"    
#>     pressure [dbar, absolute] 11.43       256.32      463.39      2633 0   "pres07"    
#>     tstamp                    7.1214e-312 7.1214e-312 7.1214e-312 2633 0   "tstamp"    
#> 
#> * Processing Log
#> 
#>     - 2025-10-21 14:21:15 UTC: `create 'rsk' object`
#>     - 2025-10-21 14:21:16 UTC: `read.rsk(file = file, encoding = encoding, allTables = FALSE,     processingLog = processingLog)`
#>     - 2025-10-21 14:21:16 UTC: `oceSetMetadata(object = rsk, name = "longitude", value = -(56 +     26.232/60))`
#>     - 2025-10-21 14:21:16 UTC: `oceSetMetadata(object = rsk, name = "latitude", value = 73 +     13.727/60)`
#>     - 2025-10-21 14:21:16 UTC: `oceSetMetadata(object = rsk, name = "station", value = "C18")`
#>     - 2025-10-21 14:21:16 UTC: `oceSetMetadata(object = rsk, name = "ship", value = "Ault")`
#>     - 2025-10-21 14:21:16 UTC: `oceSetMetadata(object = rsk, name = "institute", value = "Ocean Research Project")`
#>     - 2025-10-21 14:21:16 UTC: `subset.rsk(x, subset=1441381041 < time & time < 1441381480)`
```
