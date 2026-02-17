# Convert From POSIXt Time to Sidereal Time

Convert a POSIXt time to a sidereal time, using the method in Chapter 7
of reference 1. The small correction that he discusses after his
equation 7.1 is not applied here.

## Usage

``` r
siderealTime(t)
```

## Arguments

- t:

  a time, in POSIXt format, e.g. as created by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html),
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html), or
  [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md).
  If this is provided, the other arguments are ignored.

## Value

A sidereal time, in hours in the range from 0 to 24.

## References

- Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
  Richmond, Virginia, USA: Willmann-Bell, 1982.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

## Author

Dan Kelley

## Examples

``` r
t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz = "UTC")
print(siderealTime(t))
#> [1] 3.45037
```
