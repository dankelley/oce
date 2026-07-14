# Pretty Longitude/Latitude in Degree-Minute-Second Format

Round a geographical positions in degrees, minutes, and seconds
Depending on the range of values in `x`, rounding is done to degrees,
half-degrees, minutes, etc.

## Usage

``` r
prettyPosition(x, debug = getOption("oceDebug"))
```

## Arguments

- x:

  a series of one or more values of a latitude or longitude, in decimal
  degrees

- debug:

  set to a positive value to get debugging information during
  processing.

## Value

A vector of numbers that will yield good axis labels if
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md)
is used.

## Author

Dan Kelley

## Examples

``` r
library(oce)
formatPosition(prettyPosition(10 + 1:10 / 60 + 2.8 / 3600))
#> expression(c(10, 10, 10, 10, 10, 10, 10), c(0, 2, 4, 6, 8, 10, 
#> 12), c("N", NA, NA, NA, NA, NA, NA), NULL, NULL, NULL, NULL)
```
