# Convert Astronomical Angle in Degrees to Hours, Minutes and Seconds

The purpose of angle2hms is to facilitate comparison of `rightAscension`
angles computed by
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md)
and
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md)
with angles reported in astronomical sources and software, which often
employ an hour-minute-second notation. In that notation, decimal hour is
computed as 24/360 times the angle in degrees, and from that decimal
hour are compute integer hour and minute values, plus a decimal second
value. It is common in the astronomical literature to use strings to
represent the results, e.g. with \\11^h40^m48^s.10\\ for the value used
in the “Examples”; see Chapter 1 of Meeus (1991) for more on angle
calculation and representation.

## Usage

``` r
angle2hms(angle)
```

## Arguments

- angle:

  numerical value giving an angle in degrees

## Value

angle2hms returns a list containing values `time` (a numerical value for
decimal hour, between 0 and 24), `hour`, `minute`, and `second` (the
last of which may have a fractional part), and `string`, a character
value indicates the time in hour-minute-second notation, with the second
part to two decimal places and intervening `h`, `m` and `s` characters
between the units.

## References

- Meeus, Jean. Astronomical Algorithms. Second Edition. Richmond,
  Virginia, USA: Willmann-Bell, 1991.

## See also

Other things related to astronomy:
[`eclipticalToEquatorial()`](https://dankelley.github.io/oce/reference/eclipticalToEquatorial.md),
[`equatorialToLocalHorizontal()`](https://dankelley.github.io/oce/reference/equatorialToLocalHorizontal.md),
[`julianCenturyAnomaly()`](https://dankelley.github.io/oce/reference/julianCenturyAnomaly.md),
[`julianDay()`](https://dankelley.github.io/oce/reference/julianDay.md),
[`moonAngle()`](https://dankelley.github.io/oce/reference/moonAngle.md),
[`siderealTime()`](https://dankelley.github.io/oce/reference/siderealTime.md),
[`sunAngle()`](https://dankelley.github.io/oce/reference/sunAngle.md),
[`sunDeclinationRightAscension()`](https://dankelley.github.io/oce/reference/sunDeclinationRightAscension.md)

## Author

Dan Kelley

## Examples

``` r
# A randomly-chosen example on page 99 of Meeus (1991).
angle2hms(177.74208) # string component 11h50m58s.10
#> $hourDecimal
#> [1] 11.84947
#> 
#> $hour
#> [1] 11
#> 
#> $minute
#> [1] 50
#> 
#> $second
#> [1] 58.0992
#> 
#> $string
#> [1] "11h50m58s.10"
#> 
```
