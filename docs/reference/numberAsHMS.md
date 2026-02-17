# Convert a Numeric Time to Hour, Minute, and Second

Convert a Numeric Time to Hour, Minute, and Second

## Usage

``` r
numberAsHMS(t, default = 0)
```

## Arguments

- t:

  a vector of factors or character strings, in the format 1200 for
  12:00, 0900 for 09:00, etc.

- default:

  value to be used for the returned hour, minute and second if there is
  something wrong with the input value (e.g. its length exceeds 4
  characters, or it contains non-numeric characters)

## Value

A list containing `hour`, `minute`, and `second`, the last of which is
always zero.

## See also

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
t <- c("0900", "1234")
numberAsHMS(t)
#> $hour
#> [1]  9 12
#> 
#> $minute
#> [1]  0 34
#> 
#> $second
#> [1] 0 0
#> 
```
