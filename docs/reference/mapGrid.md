# Add a Longitude and Latitude Grid to an Existing Map

Plot longitude and latitude grid on an existing map. This is an advanced
function, requiring coordination with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) and
(possibly) also with
[`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md), and
so it is best avoided by novices, who may be satisfied with the defaults
used by
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

## Usage

``` r
mapGrid(
  dlongitude = 15,
  dlatitude = 15,
  longitude,
  latitude,
  col = "darkgray",
  lty = "solid",
  lwd = 0.5 * par("lwd"),
  polarCircle = 0,
  longitudelim,
  latitudelim,
  debug = getOption("oceDebug")
)
```

## Arguments

- dlongitude:

  increment in longitude, ignored if `longitude` is supplied, but
  otherwise determines the longitude sequence.

- dlatitude:

  increment in latitude, ignored if `latitude` is supplied, but
  otherwise determines the latitude sequence.

- longitude:

  numeric vector of longitudes, or `NULL` to prevent drawing longitude
  lines.

- latitude:

  numeric vector of latitudes, or `NULL` to prevent drawing latitude
  lines.

- col:

  color of lines

- lty:

  line type

- lwd:

  line width

- polarCircle:

  a number indicating the number of degrees of latitude extending from
  the poles, within which zones are not drawn.

- longitudelim:

  optional argument specifying suggested longitude limits for the grid.
  If this is not supplied, grid lines are drawn for the whole globe,
  which can yield excessively slow drawing speeds for small-region
  plots. This, and `latitudelim`, are both set by
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) if
  the arguments of the same name are passed to that function.

- latitudelim:

  similar to `longitudelim`.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, 2 to go two function levels deep, or 3 to go
  all the way to the core functions. Any value above 3 will be truncated
  to 3.

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html), returned
silently, containing `"side"`, `"value"`, `"type"`, and `"at"`. A
default call to
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
ensures agreement of grid and axes by using this return value in a call
to [`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md).

## Details

This is somewhat analogous to
[`grid()`](https://rdrr.io/r/graphics/grid.html), except that the first
two arguments of the latter supply the number of lines in the grid,
whereas the present function has increments for the first two arguments.

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
if (utils::packageVersion("sf") != "0.9.8") {
    # sf version 0.9-8 has a problem with this projection
    library(oce)
    data(coastlineWorld)
    par(mar = c(2, 2, 1, 1))
    # In mapPlot() call, note axes and grid args, to
    # prevent over-plotting of defaults.
    mapPlot(coastlineWorld,
        type = "l", projection = "+proj=ortho",
        axes = FALSE, grid = FALSE
    )
    mapGrid(15, 15)
}

# }
```
