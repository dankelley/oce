# Format a Latitude-Longitude Pair

Format a latitude-longitude pair, using "S" for negative latitudes, etc.

## Usage

``` r
latlonFormat(lat, lon, digits = max(6, getOption("digits") - 1))
```

## Arguments

- lat:

  latitude in \\^\circ\\N north of the equator.

- lon:

  longitude in \\^\circ\\N east of Greenwich.

- digits:

  the number of significant digits to use when printing.

## Value

A character string.

## See also

[`latFormat()`](https://dankelley.github.io/oce/reference/latFormat.md)
and
[`lonFormat()`](https://dankelley.github.io/oce/reference/lonFormat.md).

## Author

Dan Kelley
