# Coordinate Reference System Strings for Some Oceans

Create a coordinate reference string (CRS), suitable for use as a
`projection` argument to
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) or
[`plot,coastline-method()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md).

## Usage

``` r
oceCRS(region)
```

## Arguments

- region:

  character string indicating the region. This must be in the following
  list (or a string that matches to just one entry, with
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html)): `"North Atlantic"`,
  `"South Atlantic"`, `"Atlantic"`, `"North Pacific"`,
  `"South Pacific"`, `"Pacific"`, `"Arctic"`, and `"Antarctic"`.

## Value

string contain a CRS, which can be used as `projection` in
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

## Caution

This is a preliminary version of this function, with the results being
very likely to change through the autumn of 2016, guided by real-world
usage.

## See also

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
[`mapTissot()`](https://dankelley.github.io/oce/reference/mapTissot.md),
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
par(mar = c(2, 2, 1, 1))
plot(coastlineWorld, projection = oceCRS("Atlantic"), span = 12000)

plot(coastlineWorld, projection = oceCRS("North Atlantic"), span = 8000)

plot(coastlineWorld, projection = oceCRS("South Atlantic"), span = 8000)

plot(coastlineWorld, projection = oceCRS("Arctic"), span = 4000)

plot(coastlineWorld, projection = oceCRS("Antarctic"), span = 10000)

# Avoid ugly horizontal lines, an artifact of longitude shifting.
# Note: we cannot fill the land once we shift, either.
pacific <- coastlineCut(coastlineWorld, -180)
plot(pacific, proj = oceCRS("Pacific"), span = 15000, col = NULL)

plot(pacific, proj = oceCRS("North Pacific"), span = 12000, col = NULL)

plot(pacific, proj = oceCRS("South Pacific"), span = 12000, col = NULL)

# }
```
