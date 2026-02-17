# Sun Declination and Right Ascension

The formulae are from Meeus (1991), chapter 24 (which uses chapter 21).

## Usage

``` r
sunDeclinationRightAscension(time, apparent = FALSE)
```

## Arguments

- time:

  a POSIXct time. This ought to be in UTC timezone; if not, the
  behaviour of this function is unlikely to be correct.

- apparent:

  logical value indicating whether to return the 'apparent' angles.

## Value

A list containing `declination` and `rightAscension`, in degrees.

## References

- Meeus, Jean. Astronomical Algorithms. Second Edition. Richmond,
  Virginia, USA: Willmann-Bell, 1991.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md)

## Author

Dan Kelley, based on formulae in Meeus (1991).

## Examples

``` r
# Example 24.a in Meeus (1991) (page 158 PDF, 153 print)
time <- as.POSIXct("1992-10-13 00:00:00", tz = "UTC")
a <- sunDeclinationRightAscension(time, apparent = TRUE)
stopifnot(abs(a$declination - (-7.78507)) < 0.00004)
stopifnot(abs(a$rightAscension - (-161.61919)) < 0.00003)
b <- sunDeclinationRightAscension(time)
# check against previous results, to protect aginst code-drift errors
stopifnot(abs(b$declination - (-7.785464443)) < 0.000000001)
stopifnot(abs(b$rightAscension - (-161.6183305)) < 0.0000001)
```
