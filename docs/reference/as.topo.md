# Coerce Data Into a topo Object

Coerce Data Into a topo Object

## Usage

``` r
as.topo(longitude, latitude, z, filename = "")
```

## Arguments

- longitude:

  Either a vector of longitudes (in degrees east, and bounded by -180
  and 180), or a `bathy` object created by `getNOAA.bathy()` from the
  `marmap` package; in the second case, all other arguments are ignored.

- latitude:

  A vector of latitudes.

- z:

  A matrix of heights (positive over land).

- filename:

  Name of data (used when called by
  [`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md).

## Value

A [topo](https://dankelley.github.io/oce/reference/topo-class.md)
object.

## See also

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley
