# Coerce Data Into a gps Object

Coerces a sequence of longitudes and latitudes into a GPS dataset. This
may be used when
[`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md)
cannot read a file, or when the data have been manipulated.

## Usage

``` r
as.gps(longitude, latitude, filename = "")
```

## Arguments

- longitude:

  the longitude in decimal degrees, positive east of Greenwich, or a
  data frame with columns named `latitude` and `longitude`, in which
  case these values are extracted from the data frame and the second
  argument is ignored.

- latitude:

  the latitude in decimal degrees, positive north of the Equator.

- filename:

  name of file containing data (if applicable).

## Value

A [gps](https://dankelley.github.io/oce/reference/gps-class.md) object.

## See also

Other things related to gps data:
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
`[[<-,gps-method`,
[`gps-class`](https://dankelley.github.io/oce/reference/gps-class.md),
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
[`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md),
[`summary,gps-method`](https://dankelley.github.io/oce/reference/summary-gps-method.md)

## Author

Dan Kelley

## Examples

``` r
# Location of the Tower Tank at Dalhousie University
towerTank <- as.gps(-63.59428, 44.63572)
```
