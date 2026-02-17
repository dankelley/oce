# Coerce Data Into a coastline Object

Coerces a sequence of longitudes and latitudes into a coastline dataset.
This may be used when
[`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md)
cannot read a file, or when the data have been manipulated.

## Usage

``` r
as.coastline(longitude, latitude, fillable = FALSE)
```

## Arguments

- longitude:

  the longitude in decimal degrees, positive east of Greenwich, or a
  data frame with columns named `latitude` and `longitude`, in which
  case these values are extracted from the data frame and the second
  argument is ignored.

- latitude:

  the latitude in decimal degrees, positive north of the Equator.

- fillable:

  boolean indicating whether the coastline can be drawn as a filled
  polygon.

## Value

a
[coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
object.

## See also

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineBest()`](https://dankelley.github.io/oce/reference/coastlineBest.md),
[`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley
