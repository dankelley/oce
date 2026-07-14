# Cut a Coastline Object at Specified Longitude

This can be helpful in preventing
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) from
producing ugly horizontal lines in world maps. These lines occur when a
coastline segment is intersected by longitude lon_0+180. Since the
coastline files in the oce and ocedata packages are already "cut" at
longitudes of -180 and 180, the present function is not needed for
default maps, which have `+lon_0=0`. However, may help with other values
of `lon_0`.

## Usage

``` r
coastlineCut(coastline, lon_0 = 0)
```

## Arguments

- coastline:

  a
  [coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
  object.

- lon_0:

  longitude as would be given in a `+lon_0=` item in a call to
  [`sf::sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.html).

## Value

a new coastline object

## Caution

This function is provisional. Its behaviour, name and very existence may
change. Part of the development plan is to see if there is common ground
between this and the `clipPolys` function in the
[PBSmapping](https://CRAN.R-project.org/package=PBSmapping) package.

## See also

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineBest()`](https://dankelley.github.io/oce/reference/coastlineBest.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley

## Examples

``` r
# \donttest{
library(oce)
data(coastlineWorld)
mapPlot(coastlineCut(coastlineWorld, lon_0 = 100),
    projection = "+proj=moll +lon_0=100", col = "gray"
)

# }
```
