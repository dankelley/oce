# Convert X and Y to Longitude and Latitude

Convert from x-y coordinates to longitude and latitude. This is normally
called internally within oce; see “Bugs”. A projection must already have
been set up, by a call to
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) or
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md).
It should be noted that not all projections are handled well; see
“Bugs”.

## Usage

``` r
map2lonlat(x, y, init = NULL, debug = getOption("oceDebug"))
```

## Arguments

- x:

  vector containing the x component of points in the projected space, or
  a list containing items named `x` and `y`, in which case the next
  argument is ignored.

- y:

  vector containing the y coordinate of points in the projected space
  (ignored if `x` is a list, as described above).

- init:

  vector containing the initial guesses for longitude and latitude,
  presently ignored.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A list containing `longitude` and `latitude`, with `NA` values
indicating points that are off the globe as displayed.

## Bugs

`oce` uses the
[`sf::sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.html)
function to handle projections. Only those projections that have
inverses are permitted within `oce`, and of that subset, some are
omitted because the `oce` developers have experienced problems with
them.

## See also

[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md)
does the inverse operation.

A map must first have been created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
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
