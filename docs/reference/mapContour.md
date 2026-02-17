# Add Contours on a Existing map

Draw contour lines to an existing map, using
[`mapLines()`](https://dankelley.github.io/oce/reference/mapLines.md).
Note that label placement in `mapContour` is handled differently than in
[`contour()`](https://rdrr.io/r/graphics/contour.html).

## Usage

``` r
mapContour(
  longitude,
  latitude,
  z,
  nlevels = 10,
  levels = pretty(range(z, na.rm = TRUE), nlevels),
  labcex = 0.6,
  drawlabels = TRUE,
  underlay = "erase",
  col = par("fg"),
  lty = par("lty"),
  lwd = par("lwd"),
  debug = getOption("oceDebug")
)
```

## Arguments

- longitude:

  numeric vector of longitudes of points to be plotted, or an object of
  class `topo` (see
  [topo](https://dankelley.github.io/oce/reference/topo-class.md)), in
  which case `longitude`, `latitude` and `z` are inferred from that
  object. Importantly, the `longitude` system must match that of the
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  call that made the underlying plot. If not, the contours can have
  spurious lines that run across the plot. See “Dealing with longitude
  conventions” for a method of handling conflicting longitude
  conventions between
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md)
  and `mapContour()`.

- latitude:

  numeric vector of latitudes of points to be plotted.

- z:

  matrix to be contoured. The number of rows and columns in `z` must
  equal the lengths of `longitude` and `latitude`, respectively.

- nlevels:

  number of contour levels, if and only if `levels` is not supplied.

- levels:

  vector of contour levels.

- labcex:

  `cex` value used for contour labelling. As with
  [`contour()`](https://rdrr.io/r/graphics/contour.html), this is an
  absolute size, not a multiple of
  [`par`](https://rdrr.io/r/graphics/par.html)`("cex")`.

- drawlabels:

  logical value or vector indicating whether to draw contour labels. If
  the length of `drawlabels` is less than the number of levels
  specified, then [`rep()`](https://rdrr.io/r/base/rep.html) is used to
  increase the length, providing a value for each contour line. For
  those levels that are thus indicated, labels are added, at a spot
  where the contour line is closest to horizontal on the page. First,
  though, the region underneath the label is filled with the colour
  given by [`par`](https://rdrr.io/r/graphics/par.html)`("bg")`. See
  “Limitations” for notes on the status of contour labelling, and its
  limitations.

- underlay:

  character value relating to handling labels. If this equals `"erase"`
  (which is the default), then the contour line is drawn first, then the
  area under the label is erased (filled with white 'ink'), and then the
  label is drawn. This can be useful in drawing coarsely-spaced labelled
  contours on top of finely-spaced unlabelled contours. On the other
  hand, if `underlay` equals `"interrupt"`, then the contour line is
  interrupted in the region of the label, which is closer to the scheme
  used by the base
  [`contour()`](https://rdrr.io/r/graphics/contour.html) function.

- col:

  colour of the contour line, as for
  [`par`](https://rdrr.io/r/graphics/par.html)`("col")`, except here
  `col` gets lengthened by calling
  [`rep()`](https://rdrr.io/r/base/rep.html), so that individual
  contours can be coloured distinctly.

- lty:

  type of the contour line, as for
  [`par`](https://rdrr.io/r/graphics/par.html)`("lty")`, except for
  lengthening, as described for `col`.

- lwd:

  width of the contour line, as for
  [`par`](https://rdrr.io/r/graphics/par.html)`("lwd")`, except for
  lengthening, as described for `col` and `lty`.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Sample of Usage

    library(oce)
    data(coastlineWorld)
    if (requireNamespace("ocedata", quietly=TRUE)) {
        data(levitus, package = "ocedata")
        par(mar = rep(1, 4))
        mapPlot(coastlineWorld, projection = "+proj=robin", col = "lightgray")
        mapContour(levitus$longitude, levitus$latitude, levitus$SST)
    }

## Dealing with longitude conventions

Suppose a map has been plotted using longitudes that are bound between
-180 and 180. To overlay contours defined with longitude bound between 0
and 360 (as for the built-in `coastlineWorld` dataset), try Clark
Richards' method (<https://github.com/dankelley/oce/issues/2217>, as
below.

    # Start with z=z(lon,lat), with lon bound by 0 and 360
    z2 <- rbind(z[lon > 180, ], z[lon <= 180, ])
    lon2 <- lon + 180
    mapContour(lon2, lat, z2)

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
