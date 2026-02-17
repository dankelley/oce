# Add a Scalebar to a Map

Draw a scalebar on a map created by
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) or
[`plot,coastline-method()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md).
For plots made otherwise, see ‘Details’.

## Usage

``` r
mapScalebar(
  x,
  y = NULL,
  length,
  lwd = 1.5 * par("lwd"),
  cex = par("cex"),
  col = "black",
  debug = getOption("oceDebug")
)
```

## Arguments

- x, y:

  position of the scalebar. Eventually these two items may be handled
  similarly to the corresponding arguments in
  [`legend()`](https://rdrr.io/r/graphics/legend.html), but at the
  moment `y` must be `NULL` and `x` must be `"top"`, `"topleft"` or
  `"topright"`.

- length:

  the distance to indicate, in kilometres. If not provided, a reasonable
  choice is made, based on the existing plot.

- lwd:

  line width of the scalebar.

- cex:

  character expansion factor for the scalebar text.

- col:

  color of the scalebar.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

It is also possible to use this with any plot, but the results will only
make sense if the aspect ratio is set properly so that an item that is
circular in nature is (very closely) circular on the plot. This aspect
ratio condition is met by both
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md) and
[`plot,coastline-method()`](https://dankelley.github.io/oce/reference/plot-coastline-method.md).

The scale is appropriate to the centre of the plot, and will become
increasingly inaccurate away from that spot, with the error depending on
the projection and the fraction of the earth that is shown.

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
# Arctic Ocean
par(mar = c(2.5, 2.5, 1, 1))
mapPlot(coastlineWorld,
    latitudelim = c(60, 120), longitudelim = c(-130, -50),
    col = "lightgray", projection = "+proj=stere +lat_0=90"
)
mapScalebar()

# }

```
