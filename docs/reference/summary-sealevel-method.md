# Summarize a sealevel Object

Summarizes some of the data in a sealevel object.

## Usage

``` r
# S4 method for class 'sealevel'
summary(object, ...)
```

## Arguments

- object:

  A
  [sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
  object.

- ...:

  further arguments passed to or from other methods.

## Value

A matrix containing statistics of the elements of the `data` slot.

## See also

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`read.sealevel.gc2026()`](https://dankelley.github.io/oce/reference/read.sealevel.gc2026.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(sealevel)
summary(sealevel)
#> Sealevel Summary
#> ----------------
#> 
#> * number:              490
#> * name:                HALIFAX
#> * sampling delta-t:    1 hour
#> * Location:            44.667N 63.583W 
#> * year:                2003
#> * number of observations:   6659 
#> *    "      non-missing:    6659 
#> * Time: 2003-01-01 13:00:00 to 2003-10-08 11:00:00 (6659 samples, mean increment 1.009012 hour)
#> * Data Overview
#> 
#>                   Min.       Mean       Max.       Dim. NAs
#>     elevation [m] 0          0.98622    2.84       6659 0  
#>     time          1041426000 1053505272 1065610800 6659 0  
#> 
#> * Processing Log
#> 
#>     - 2016-05-05 18:33:53 UTC: `create 'sealevel' object`
#>     - 2016-05-05 18:33:53 UTC: `read.sealevel(file="490-01-JAN-2003_slev.csv", tz="UTC")`
#>     - 2016-05-05 18:33:53 UTC: `oce.edit(x = sealevel, item = "longitude", value = -sealevel[["longitude"]],     reason = "Fix longitude hemisphere")`
```
