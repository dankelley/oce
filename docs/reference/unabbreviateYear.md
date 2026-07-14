# Determine Year From Various Abbreviations

Various data files may contain various abbreviations for years. For
example, 99 refers to 1999, and 8 refers to 2008. Sometimes, even 108
refers to 2008 (the idea being that the "zero" year was 1900). This
function deals with the three cases mentioned. It will fail if someone
supplies 60, meaning year 2060 as opposed to 1960.

## Usage

``` r
unabbreviateYear(year)
```

## Arguments

- year:

  a year, or vector of years, possibly abbreviated

## See also

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md)

## Author

Dan Kelley

## Examples

``` r
fullYear <- unabbreviateYear(c(99, 8, 108))
```
