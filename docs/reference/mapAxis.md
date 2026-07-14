# Add Axis Labels to an Existing Map

Plot axis labels on an existing map. This is an advanced function,
requiring coordination with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) and
(possibly) also with
[`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md), and
so it is best avoided by novices, who may be satisfied with the defaults
used by
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

## Usage

``` r
mapAxis(
  side = 1:2,
  longitude = TRUE,
  latitude = TRUE,
  axisStyle = 1,
  tick = TRUE,
  line = NA,
  pos = NA,
  outer = FALSE,
  font = NA,
  las = c(0, 0),
  lty = "solid",
  lwd = 1,
  lwd.ticks = lwd,
  col = NULL,
  col.ticks = NULL,
  hadj = NA,
  padj = NA,
  tcl = -0.3,
  cex.axis = 1,
  mgp = c(0, 0.5, 0),
  debug = getOption("oceDebug")
)
```

## Arguments

- side:

  the side at which labels are to be drawn. If not provided, sides 1 and
  2 will be used (i.e. bottom and left-hand sides).

- longitude:

  either a logical value or a numeric vector of longitudes. There are
  three possible cases: (1) If `longitude=TRUE` (the default) then ticks
  and nearby numbers will occur at the longitude grid established by the
  previous call to
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md); (2)
  if `longitude=FALSE` then no longitude ticks or numbers are drawn; (3)
  if `longitude` is a vector of numerical values, then those ticks are
  placed at those values, and numbers are written beside them. Note that
  in cases 1 and 3, efforts are made to avoid overdrawing text, so some
  longitude values might get ticks but not numbers. To get ticks but not
  numbers, set `cex.axis=0`.

- latitude:

  similar to `longitude` but for latitude.

- axisStyle:

  an integer specifying the style of labels for the numbers on axes. The
  choices are: 1 for signed numbers without additional labels; 2 (the
  default) for unsigned numbers followed by letters indicating the
  hemisphere; 3 for signed numbers followed by a degree sign; 4 for
  unsigned numbers followed by a degree sign; and 5 for signed numbers
  followed by a degree sign and letters indicating the hemisphere.

- tick:

  parameter passed to [`axis()`](https://rdrr.io/r/graphics/axis.html).

- line:

  parameter passed to [`axis()`](https://rdrr.io/r/graphics/axis.html).

- pos:

  parameter passed to [`axis()`](https://rdrr.io/r/graphics/axis.html).

- outer:

  parameter passed to [`axis()`](https://rdrr.io/r/graphics/axis.html).

- font:

  axis font, passed to [`axis()`](https://rdrr.io/r/graphics/axis.html).

- las:

  two-element axis label orientation, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html). The first value is
  for the horizontal axis, and the second is for the vertical axis. See
  [`par()`](https://rdrr.io/r/graphics/par.html) for the meanings of the
  permitted values, namely 0, 1, 2 and 3.

- lty:

  axis line type, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- lwd:

  axis line width, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html)).

- lwd.ticks:

  tick line width, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- col:

  axis color, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- col.ticks:

  axis tick color, passed to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- hadj:

  an argument that is transmitted to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- padj:

  an argument that is transmitted to
  [`axis()`](https://rdrr.io/r/graphics/axis.html).

- tcl:

  axis-tick size (see [`par()`](https://rdrr.io/r/graphics/par.html)).

- cex.axis:

  axis-label expansion factor (see
  [`par()`](https://rdrr.io/r/graphics/par.html)); set to 0 to prevent
  numbers from being placed in axes.

- mgp:

  three-element numerical vector describing axis-label placement (see
  [`par()`](https://rdrr.io/r/graphics/par.html)). It usually makes
  sense to set the first and third elements to zero.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## See also

A map must first have been created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md),
[`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md),
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
par(mar = c(2, 2, 1, 1))
lonlim <- c(-180, 180)
latlim <- c(70, 110)
# In mapPlot() call, note axes and grid args, to
# prevent over-plotting of defaults.  Some adjustments
# might be required to the mapGrid() arguments, to
# get agreement with the axis. This is why both
# mapGrid() and mapAxis() are best avoided; it is
# simpler to let mapPlot() handle these things.
mapPlot(coastlineWorld,
    projection = "+proj=stere +lat_0=90",
    longitudelim = lonlim, latitudelim = latlim,
    col = "tan", axes = FALSE, grid = FALSE
)
mapGrid(15, 15)
mapAxis(axisStyle = 5)

# }
```
