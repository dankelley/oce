# Add Arrows to a Map

Plot arrows on an existing map, e.g. to indicate a place location. This
is not well-suited for drawing direction fields, e.g. of velocities; for
that, see
[`mapDirectionField()`](https://dankelley.github.io/oce/reference/mapDirectionField.md).
Adds arrows to an existing map, by analogy to
[`arrows()`](https://rdrr.io/r/graphics/arrows.html).

## Usage

``` r
mapArrows(
  longitude0,
  latitude0,
  longitude1 = longitude0,
  latitude1 = latitude0,
  length = 0.25,
  angle = 30,
  code = 2,
  col = par("fg"),
  lty = par("lty"),
  lwd = par("lwd"),
  ...
)
```

## Arguments

- longitude0, latitude0:

  starting points for arrows.

- longitude1, latitude1:

  ending points for arrows.

- length:

  length of the arrow heads, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- angle:

  angle of the arrow heads, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- code:

  numerical code indicating the type of arrows, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- col:

  arrow color, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- lty:

  arrow line type, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- lwd:

  arrow line width, passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

- ...:

  optional arguments passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html).

## See also

A map must first have been created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md),
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
[`mapTissot()`](https://dankelley.github.io/oce/reference/mapTissot.md),
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
mapPlot(coastlineWorld,
    longitudelim = c(-120, -60), latitudelim = c(30, 60),
    col = "lightgray", projection = "+proj=lcc +lat_1=45 +lon_0=-100"
)
lon <- seq(-120, -75, 15)
n <- length(lon)
lat <- 45 + rep(0, n)
# Draw meridional arrows in N America, from 45N to 60N.
mapArrows(lon, lat, lon, lat + 15, length = 0.05, col = "blue")

# }
```
