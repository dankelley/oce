# Draw a Coordinate System

Draws arrows on a map to indicate a coordinate system, e.g. for an to
indicate a coordinate system set up so that one axis is parallel to a
coastline.

## Usage

``` r
mapCoordinateSystem(longitude, latitude, L = 100, phi = 0, ...)
```

## Arguments

- longitude:

  numeric vector of longitudes in degrees.

- latitude:

  numeric vector of latitudes in degrees.

- L:

  axis length in km.

- phi:

  angle, in degrees counterclockwise, that the "x" axis makes to a line
  of latitude.

- ...:

  plotting arguments, passed to
  [`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md);
  see “Examples” for how to control the arrow-head size.

## Details

This is a preliminary version of this function. It only works if the
lines of constant latitude are horizontal on the plot.

## Sample of Usage


    library(oce)
    if (requireNamespace("ocedata", quietly=TRUE)) {
        data(coastlineWorldFine, package="ocedata")
        HfxLon <- -63.5752
        HfxLat <- 44.6488
        mapPlot(coastlineWorldFine, proj="+proj=merc",
            longitudelim=HfxLon+c(-2,2), latitudelim=HfxLat+c(-2,2),
            col=lightgrey")
        mapCoordinateSystem(HfxLon, HfxLat, phi=45, length=0.05)
       }

## See also

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md),
[`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md),
[`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md),
[`mapContour()`](https://dankelley.github.io/oce/reference/mapContour.md),
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

Chantelle Layton
