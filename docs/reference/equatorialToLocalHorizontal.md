# Convert Equatorial Coordinate to Local Horizontal Coordinate

Convert from equatorial coordinates to local horizontal coordinates,
i.e. azimuth and altitude. The method is taken from equations 8.5 and
8.6 of reference 1, or, equivalently, from equations 12.5 and 12.6 of
reference 2.

## Usage

``` r
equatorialToLocalHorizontal(
  rightAscension,
  declination,
  t,
  longitude,
  latitude
)
```

## Arguments

- rightAscension:

  right ascension, e.g. calculated with
  [`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md).

- declination:

  declination, e.g. calculated with
  [`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md).

- t:

  time of observation.

- longitude:

  longitude of observation, positive in eastern hemisphere.

- latitude:

  latitude of observation, positive in northern hemisphere.

## Value

A data frame containing columns `altitude` (angle above horizon, in
degrees) and `azimuth` (angle anticlockwise from south, in degrees).

## References

- Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
  Richmond, Virginia, USA: Willmann-Bell, 1982.

- Meeus, Jean. Astronomical Algorithms. Second Edition. Richmond,
  Virginia, USA: Willmann-Bell, 1991.

## See also

Other things related to astronomy:
[`angle2hms()`](https://dankelley.github.io/oce/reference/angle2hms.md),
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

## Author

Dan Kelley, based on formulae in references 1 and 2.
