# Add a Polygon to a Map

`mapPolygon` adds a polygon to an existing map.

## Usage

``` r
mapPolygon(
  longitude,
  latitude,
  density = NULL,
  angle = 45,
  border = NULL,
  col = NA,
  lty = par("lty"),
  ...,
  fillOddEven = FALSE
)
```

## Arguments

- longitude:

  numeric vector of longitudes of points defining the polygon, to be
  plotted, or an object from which both longitude and latitude can be
  inferred (e.g. a coastline file, or the return value from
  [`mapLocator()`](https://dankelley.github.io/oce/reference/mapLocator.md)),
  in which case the `latitude` argument are ignored.

- latitude:

  numeric vector of latitudes of points to be plotted (ignored if both
  longitude and latitude can be determined from the first argument).

- density, angle, border, col, lty, ..., fillOddEven:

  handled as [`polygon()`](https://rdrr.io/r/graphics/polygon.html)
  handles the same arguments.

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
data(topoWorld)

# Bathymetry near southeastern Canada
par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
cm <- colormap(zlim = c(-5000, 0), col = oceColorsGebco)
drawPalette(colormap = cm)
lonlim <- c(-60, -50)
latlim <- c(40, 60)
mapPlot(coastlineWorld,
    longitudelim = lonlim,
    latitudelim = latlim, projection = "+proj=merc", grid = FALSE
)
mapImage(topoWorld, colormap = cm)
mapPolygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col = "lightgray")

# }
```
