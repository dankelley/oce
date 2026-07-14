# Convert Longitude and Latitude to X and Y

If a projection is already being used (e.g. as set by
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md))
then only `longitude` and `latitude` should be given, and the other
arguments will be inferred by `lonlat2map`. This is important because
otherwise, if a new projection is called for, it will ruin any additions
to the existing plot.

## Usage

``` r
lonlat2map(longitude, latitude, projection = "", debug = getOption("oceDebug"))
```

## Arguments

- longitude:

  one of three choices: (1) a numeric vector containing decimal
  longitudes, (2) a list containing items named `longitude` and
  `latitude`, or (3) a
  [coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
  object, e.g. as created with
  [`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md)
  or
  [`read.coastline()`](https://dankelley.github.io/oce/reference/read.coastline.md).
  In the second two cases, the values of longitude and latitude are
  inferred from the first argument, and any supplied value of `latitude`
  (next parameter) is ignored.

- latitude:

  a numeric vector containing decimal latitude (ignored if `longitude`
  is a list, as described above).

- projection:

  optional indication of projection. This must be character string in
  the format used by the [sf](https://CRAN.R-project.org/package=sf)
  package; see
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).)

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A list containing `x` and `y`.

## See also

`mapLongitudeLatitudeXY` is a safer alternative, if a map has already
been drawn with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md),
because that function cannot alter an existing projection.
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md)
is an inverse to `map2lonlat`.

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
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
[`shiftLongitude()`](https://dankelley.github.io/oce/reference/shiftLongitude.md),
[`usrLonLat()`](https://dankelley.github.io/oce/reference/usrLonLat.md),
[`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude = -64.49657, latitude = 45.33462)
xy <- lonlat2map(cs, projection = "+proj=merc")
map2lonlat(xy)
#> $longitude
#> [1] -64.49657
#> 
#> $latitude
#> [1] 45.33462
#> 
```
