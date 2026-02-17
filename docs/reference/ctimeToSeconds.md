# Interpret a Character String as a Time Interval

Infer a time interval from a character string in the form MM:SS or
HH:MM:SS.

## Usage

``` r
ctimeToSeconds(ctime)
```

## Arguments

- ctime:

  a character string (see “Details”.

## Value

A numeric value, the number of seconds represented by the string.

## See also

See
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
the inverse of this.

Other things related to time:
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
cat("10      = ", ctimeToSeconds("10"), "s\n", sep = "")
#> 10      = 10s
cat("01:04   = ", ctimeToSeconds("01:04"), "s\n", sep = "")
#> 01:04   = 64s
cat("1:00:00 = ", ctimeToSeconds("1:00:00"), "s\n", sep = "")
#> 1:00:00 = 3600s
```
