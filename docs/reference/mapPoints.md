# Add Points to a Map

Plot points on an existing map, by analogy to
[`points()`](https://rdrr.io/r/graphics/points.html).

## Usage

``` r
mapPoints(longitude, latitude, debug = getOption("oceDebug"), ...)
```

## Arguments

- longitude:

  Longitudes of points to be plotted, or an object from which longitude
  and latitude can be inferred in which case the following two arguments
  are ignored. This objects that are possible include those of type
  `coastline`.

- latitude:

  numeric vector of latitudes of points to be plotted.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  Optional arguments passed to
  [`points()`](https://rdrr.io/r/graphics/points.html).

## See also

A map must first have been created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

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
    longitudelim = c(-80, 0), latitudelim = c(20, 50),
    col = "lightgray", projection = "+proj=laea +lon_0=-35"
)
data(section)
mapPoints(section)

# }
```
