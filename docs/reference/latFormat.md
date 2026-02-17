# Format a Latitude

Format a latitude, using "S" for negative latitude.

## Usage

``` r
latFormat(lat, digits = max(6, getOption("digits") - 1))
```

## Arguments

- lat:

  latitude in \\^\circ\\N north of the equator.

- digits:

  the number of significant digits to use when printing.

## Value

A character string.

## See also

[`lonFormat()`](https://dankelley.github.io/oce/reference/lonFormat.md)
and
[`latlonFormat()`](https://dankelley.github.io/oce/reference/latlonFormat.md).

## Author

Dan Kelley
