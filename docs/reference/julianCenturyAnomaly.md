# Convert Julian-Day-Number to Julian Century

Convert a Julian-Day number to a time in julian centuries since noon on
January 1, 1900. The method follows Equation 15.1 in Reference 1. The
example reproduces the Example 15.a of the same source, with fractional
error 3e-8.

## Usage

``` r
julianCenturyAnomaly(jd)
```

## Arguments

- jd:

  a julian day number, e.g. as given by
  [`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md).

## Value

Julian century since noon on January 1, 1900.

## References

1.  Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
    Richmond, Virginia, USA: Willmann-Bell, 1982.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

Other things related to time:
[`ctimeToSeconds()`](https://dankelley.github.io/oce/reference/ctimeToSeconds.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`numberAsHMS()`](https://dankelley.github.io/oce/reference/numberAsHMS.md),
[`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
[`secondsToCtime()`](https://dankelley.github.io/oce/reference/secondsToCtime.md),
[`unabbreviateYear()`](https://dankelley.github.io/oce/reference/unabbreviateYear.md)

## Author

Dan Kelley

## Examples

``` r
t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz = "UTC")
jca <- julianCenturyAnomaly(julianDay(t))
cat(format(t), "is Julian Century anomaly", format(jca, digits = 8), "\n")
#> 1978-11-13 04:35:00 is Julian Century anomaly 0.78865684 
```
