# Shift Longitude to Range -180 to 180

This is a utility function used by
[`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md). It
works simply by subtracting 180 from each longitude, if any longitude in
the vector exceeds 180.

## Usage

``` r
shiftLongitude(longitudes)
```

## Arguments

- longitudes:

  numerical vector of longitudes.

## Value

vector of longitudes, shifted to the desired range.

## See also

[`matrixShiftLongitude()`](https://dankelley.github.io/oce/reference/matrixShiftLongitude.md)
and
[`standardizeLongitude()`](https://dankelley.github.io/oce/reference/standardizeLongitude.md).

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
[`oceCRS()`](https://dankelley.github.io/oce/reference/oceCRS.md),
[`oceProject()`](https://dankelley.github.io/oce/reference/oceProject.md),
[`usrLonLat()`](https://dankelley.github.io/oce/reference/usrLonLat.md),
[`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)

## Author

Dan Kelley
