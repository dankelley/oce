# Add Tissot Indicatrices to a Map

Plot ellipses at grid intersection points, as a method for indicating
the distortion inherent in the projection, somewhat analogous to the
scheme used in reference 1. (Each ellipse is drawn with 64 segments.)

## Usage

``` r
mapTissot(grid = rep(15, 2), scale = 0.2, crosshairs = FALSE, ...)
```

## Arguments

- grid:

  numeric vector of length 2, specifying the increment in longitude and
  latitude for the grid. Indicatrices are drawn at e.g. longitudes
  `seq(-180, 180, grid[1])`.

- scale:

  numerical scale factor for ellipses. This is multiplied by `min(grid)`
  and the result is the radius of the circle on the earth, in latitude
  degrees.

- crosshairs:

  logical value indicating whether to draw constant-latitude and
  constant-longitude crosshairs within the ellipses. (These are drawn
  with 10 line segments each.) This can be helpful in cases where it is
  not desired to use
  [`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md) to
  draw the longitude/latitude grid.

- ...:

  extra arguments passed to plotting functions, e.g. `col="red"` yields
  red indicatrices.

## References

1.  Snyder, John P., 1987. Map Projections: A Working Manual. USGS
    Professional Paper: 1395

## See also

A map must first have been created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md),
[`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md),
[`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md),
[`mapContour()`](https://dankelley.github.io/oce/reference/mapContour.md),
[`mapCoordinateSystem()`](https://dankelley.github.io/oce/reference/mapCoordinateSystem.md),
[`mapDirectionField()`](https://dankelley.github.io/oce/reference/mapDirectionField.md),
[`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md),
[`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md),
[`mapLines()`](https://dankelley.github.io/oce/reference/mapLines.md),
[`mapLocator()`](https://dankelley.github.io/oce/reference/mapLocator.md),
[`mapLongitudeLatitudeXY()`](https://dankelley.github.io/oce/reference/mapLongitudeLatitudeXY.md),
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md),
[`mapPoints()`](https://dankelley.github.io/oce/reference/mapPoints.md),
[`mapPolygon()`](https://dankelley.github.io/oce/reference/mapPolygon.md),
[`mapScalebar()`](https://dankelley.github.io/oce/reference/mapScalebar.md),
[`mapText()`](https://dankelley.github.io/oce/reference/mapText.md),
[`oceCRS()`](https://dankelley.github.io/oce/reference/oceCRS.md),
[`oceProject()`](https://dankelley.github.io/oce/reference/oceProject.md),
[`shiftLongitude()`](https://dankelley.github.io/oce/reference/shiftLongitude.md),
[`usrLonLat()`](https://dankelley.github.io/oce/reference/usrLonLat.md),
[`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)

## Author

Dan Kelley

## Examples

``` r
# \donttest{
library(oce)
data(coastlineWorld)
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
p <- "+proj=aea +lat_1=10 +lat_2=60 +lon_0=-45"
mapPlot(coastlineWorld,
    projection = p, col = "gray",
    longitudelim = c(-90, 0), latitudelim = c(0, 50)
)
mapTissot(c(15, 15), col = "red")

# }
```
