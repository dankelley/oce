# Format a Longitude

Format a longitude, using "W" for west longitude.

## Usage

``` r
lonFormat(lon, digits = max(6, getOption("digits") - 1))
```

## Arguments

- lon:

  longitude in \\^\circ\\N east of Greenwich.

- digits:

  the number of significant digits to use when printing.

## Value

A character string.

## See also

[`latFormat()`](https://dankelley.github.io/oce/reference/latFormat.md)
and
[`latlonFormat()`](https://dankelley.github.io/oce/reference/latlonFormat.md).

## Author

Dan Kelley
