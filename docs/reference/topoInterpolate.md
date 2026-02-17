# Interpolate Within a topo Object

Bilinear interpolation is used so that values will vary smoothly within
a longitude-latitude grid cell. Note that the sign convention for
`longitude` and `latitude` must match that in `topo`.

## Usage

``` r
topoInterpolate(longitude, latitude, topo)
```

## Arguments

- longitude:

  Vector of longitudes (in the same sign convention as used in `topo`).

- latitude:

  Vector of latitudes (in the same sign convention as used in `topo`).

- topo:

  A [topo](https://dankelley.github.io/oce/reference/topo-class.md)
  object.

## Value

Vector of heights giving the elevation of the earth above means sea
level at the indicated location on the earth.

## See also

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(topoWorld)
# "The Gully", approx. 400m deep, connects Gulf of St Lawrence with North Atlantic
topoInterpolate(45, -57, topoWorld)
#> [1] -5208.5
```
