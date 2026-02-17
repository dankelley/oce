# Express Time Interval as Colon-Separated String

Convert a time interval to a colon-separated string

## Usage

``` r
secondsToCtime(sec)
```

## Arguments

- sec:

  length of time interval in seconds.

## Value

A string with a colon-separated time interval.

## See also

See
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
the inverse of this.

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
cat("   10 s = ", secondsToCtime(10), "\n", sep = "")
#>    10 s = 00:00:10
cat("   61 s = ", secondsToCtime(61), "\n", sep = "")
#>    61 s = 00:01:01
cat("86400 s = ", secondsToCtime(86400), "\n", sep = "")
#> 86400 s = 24:00:00
```
