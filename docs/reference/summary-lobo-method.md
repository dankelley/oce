# Summarize a lobo Object

Pertinent summary information is presented, including the sampling
interval, data ranges, etc.

## Usage

``` r
# S4 method for class 'lobo'
summary(object, ...)
```

## Arguments

- object:

  a [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
  object.

- ...:

  further arguments passed to or from other methods.

## Value

A matrix containing statistics of the elements of the `data` slot.

## See also

The documentation for
[lobo](https://dankelley.github.io/oce/reference/lobo-class.md) explains
the structure of LOBO objects, and also outlines the other functions
dealing with them.

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`as.lobo()`](https://dankelley.github.io/oce/reference/as.lobo.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(lobo)
summary(lobo)
#> Lobo Summary
#> ------------
#> 
#> * source: "/Users/kelley/src/oce/create_data/lobo/lobo.dat"
#> * Time: 2009-03-01 to 2009-04-01 23:00:00 (768 samples, mean increment 1 hour)
#> * Data Overview
#> 
#>                                 Min.  Mean     Max. Dim. NAs
#>     time                        NA    NA       NA   768  0  
#>     u [m/s]                     NA    NA       NA   768  768
#>     v [m/s]                     NA    NA       NA   768  768
#>     salinity [PSS-78]           13.61 29.208   30.8 768  0  
#>     temperature [°C, ITS-90]    -0.72 1.691    4.55 768  0  
#>     airtemperature [°C, ITS-90] -10.4 -0.20573 11.8 768  0  
#>     pressure [dbar]             NA    NA       NA   768  768
#>     nitrate [μM]                0.3   6.7108   38.5 768  0  
#>     fluorescence [μg/l]         0.32  1.2463   4.83 768  0  
#> 
#> * Processing Log
#> 
#>     - 2016-01-10 15:34:18 UTC: `create 'lobo' object`
#>     - 2016-01-10 15:34:18 UTC: `read.lobo(file = "lobo.dat")`
#>     - 2016-01-10 15:34:19 UTC: `subset.lobo(x, subset=start <= time & time <= end)`
#>     - 2016-01-10 15:34:19 UTC: `subset.lobo(x, subset=!is.na(temperature))`
```
