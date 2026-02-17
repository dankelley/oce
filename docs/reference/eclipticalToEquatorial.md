# Convert Ecliptical Coordinate to Equatorial Coordinate

Convert from ecliptical to equatorial coordinates, using equations 8.3
and 8.4 of reference 1, or, equivalently, equations 12.3 and 12.4 of
reference 2.

## Usage

``` r
eclipticalToEquatorial(lambda, beta, epsilon)
```

## Arguments

- lambda:

  longitude, in degrees, or a data frame containing `lambda`, `beta`,
  and `epsilon`, in which case the next to arguments are ignored

- beta:

  geocentric latitude, in degrees

- epsilon:

  obliquity of the ecliptic, in degrees

## Value

A data frame containing columns `rightAscension` and `declination` both
in degrees.

## Details

The code is based on reference 1; see
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md)
for comments on the differences in formulae found in reference 2.
Indeed, reference 2 is only cited here in case readers want to check the
ideas of the formulae; DK has found that reference 2 is available to him
via his university library inter-library loan system, whereas he owns a
copy of reference 1.

## References

- Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
  Richmond, Virginia, USA: Willmann-Bell, 1982.

- Meeus, Jean. Astronomical Algorithms. Second Edition. Richmond,
  Virginia, USA: Willmann-Bell, 1991.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

## Author

Dan Kelley, based on formulae in references 1 and 2.
