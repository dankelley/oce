# Convert a Time to a Julian Day

Convert a POSIXt time (given as either the `t` argument or as the
`year`, `month`, and other arguments) to a Julian day, using the method
provided in Chapter 3 of Meeus (1982). It should be noted that Meeus and
other astronomical treatments use fractional days, whereas the present
code follows the R convention of specifying days in whole numbers, with
hours, minutes, and seconds also provided as necessary. Conversion is
simple, as illustrated in the example for 1977 April 26.4, for which
Meeus calculates julian day 2443259.9. Note that the R documentation for
[`julian()`](https://rdrr.io/r/base/weekday.POSIXt.html) suggests
another formula, but the point of the present function is to match the
other Meeus formulae, so that suggestion is ignored here.

## Usage

``` r
julianDay(
  t,
  year = NA,
  month = NA,
  day = NA,
  hour = NA,
  min = NA,
  sec = NA,
  tz = "UTC"
)
```

## Arguments

- t:

  a time, in POSIXt format, e.g. as created by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html),
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html), or
  [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
  or a character string that can be converted to a time using
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html). If `t` is
  provided, the other arguments are ignored.

- year:

  year, to be provided along with `month`, etc., if `t` is not provided.

- month:

  numerical value for the month, with January being 1. (This is required
  if `t` is not provided.)

- day:

  numerical value for day in month, starting at 1. (This is required if
  `t` is not provided.)

- hour:

  numerical value for hour of day, in range 0 to 24. (This is required
  if `t` is not provided.)

- min:

  numerical value of the minute of the hour. (This is required if `t` is
  not provided.)

- sec:

  numerical value for the second of the minute. (This is required if `t`
  is not provided.)

- tz:

  timezone

## Value

A Julian-Day number, in astronomical convention as explained in Meeus.

## References

- Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
  Richmond, Virginia, USA: Willmann-Bell, 1982.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# example from Meeus
t <- ISOdatetime(1977, 4, 26, hour = 0, min = 0, sec = 0, tz = "UTC") + 0.4 * 86400
stopifnot(all.equal(julianDay(t), 2443259.9))
```
