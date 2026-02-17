# Add Lines to a Map

Plot lines on an existing map, by analogy to
[`lines()`](https://rdrr.io/r/graphics/lines.html).

## Usage

``` r
mapLines(longitude, latitude, greatCircle = FALSE, ...)
```

## Arguments

- longitude:

  numeric vector of longitudes of points to be plotted, or an object
  from which longitude and latitude can be inferred (e.g. a coastline
  file, or the return value from
  [`mapLocator()`](https://dankelley.github.io/oce/reference/mapLocator.md)),
  in which case the following two arguments are ignored.

- latitude:

  vector of latitudes of points to be plotted.

- greatCircle:

  a logical value indicating whether to render line segments as great
  circles. (Ignored.)

- ...:

  optional arguments passed to
  [`lines()`](https://rdrr.io/r/graphics/lines.html).

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
if (utils::packageVersion("sf") != "0.9.8") {
    # sf version 0.9-8 has a problem with this projection
    library(oce)
    data(coastlineWorld)
    mapPlot(coastlineWorld,
        type = "l",
        longitudelim = c(-80, 10), latitudelim = c(0, 120),
        projection = "+proj=ortho +lon_0=-40"
    )
    lon <- c(-63.5744, 0.1062) # Halifax CA to London UK
    lat <- c(44.6479, 51.5171)
    mapPoints(lon, lat, col = "red")
    mapLines(lon, lat, col = "red")
}

# }
```
