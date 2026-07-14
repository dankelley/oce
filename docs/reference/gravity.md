# Acceleration Due to Earth Gravity

Compute \\g\\, the acceleration due to gravity, as a function of
latitude.

## Usage

``` r
gravity(latitude = 45, degrees = TRUE)
```

## Arguments

- latitude:

  Latitude in \\^\circ\\N or radians north of the equator.

- degrees:

  Flag indicating whether degrees are used for latitude; if set to
  `FALSE`, radians are used.

## Value

Acceleration due to gravity, in \\m^2/s\\.

## Details

Value not verified yet, except roughly.

## References

Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New York,
662 pp.

**Caution:** Fofonoff and Millard (1983 UNESCO) use a different formula.

## Author

Dan Kelley

## Examples

``` r
g <- gravity(45) # 9.8
```
