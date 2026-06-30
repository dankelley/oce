# Add an Image to a Map

Plot an image on an existing map that was created with
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

## Usage

``` r
mapImage(
  longitude,
  latitude,
  z,
  zlim,
  zclip = FALSE,
  breaks,
  col,
  colormap,
  border = NA,
  lwd = par("lwd"),
  lty = par("lty"),
  missingColor = NA,
  filledContour = FALSE,
  gridder = "binMean2D",
  debug = getOption("oceDebug")
)
```

## Arguments

- longitude:

  numeric vector of longitudes corresponding to `z` matrix.

- latitude:

  numeric vector of latitudes corresponding to `z` matrix.

- z:

  numeric matrix to be represented as an image.

- zlim:

  limit for z (color).

- zclip:

  A logical value, `TRUE` indicating that out-of-range `z` values should
  be painted with `missingColor` and `FALSE` indicating that these
  values should be painted with the nearest in-range color. If `zlim` is
  given then its min and max set the range. If `zlim` is not given but
  `breaks` is given, then the min and max of `breaks` sets the range
  used for z. If neither `zlim` nor `breaks` is given, clipping is not
  done, i.e. the action is as if `zclip` were `FALSE`.

- breaks:

  The z values for breaks in the color scheme. If this is of length 1,
  the value indicates the desired number of breaks, which is supplied to
  [`pretty()`](https://rdrr.io/r/base/pretty.html), in determining clean
  break points.

- col:

  Either a vector of colors corresponding to the breaks, of length 1
  plus the number of breaks, or a function specifying colors, e.g.
  [`oce.colorsViridis()`](https://dankelley.github.io/oce/reference/oceColorsViridis.md)
  for the Viridis scheme.

- colormap:

  optional colormap, as created by
  [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md).
  If a `colormap` is provided, then its properties takes precedence over
  `breaks`, `col`, `missingColor`, and `zclip` specified to `mapImage`.

- border:

  Color used for borders of patches (passed to
  [`polygon()`](https://rdrr.io/r/graphics/polygon.html)); the default
  `NA` means no border.

- lwd:

  line width, used if borders are drawn.

- lty:

  line type, used if borders are drawn.

- missingColor:

  a color to be used to indicate missing data, or `NA` to skip the
  drawing of such regions (which will retain whatever material has
  already been drawn at the regions).

- filledContour:

  an indication of whether to use filled contours. This may be FALSE
  (the default), TRUE, or a positive numerical value. If FALSE, then
  polygons are used. Otherwise, the longitude-latitude values are
  transformed to x-y values, which will not be on a grid and thus will
  require gridding so that
  [`.filled.contour()`](https://rdrr.io/r/graphics/filled.contour.html)
  can plot the filled contours. The method used for gridding is set by
  the `gridder` parameter (see next item). If `filledContour` is TRUE,
  then the grid is constructed with the aim of having approximately 3 of
  the projected x-y points in each cell. That can leave some cells
  unoccupied, yielding blanks in the drawn image. There are two ways
  around that. First, the `gridder` can be set up to fill gaps. Second,
  a numerical value can be used for `filledContour`. For example, using
  `filledContour` equal to 1.5 will increase grid width and height by a
  factor of 1.5, which may be enough to fill all the gaps, depending on
  the projection and the area shown.

- gridder:

  specification of how gridding is to be done, used only if
  `filledContour` is TRUE. The value of `gridder` may `"binMean2D"`,
  which is the default, `"interp"`, or a function. In the first two
  cases, the gridding is done with either
  [`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)
  or [`interp::interp()`](https://rdrr.io/pkg/interp/man/interp.html),
  respectively. For more on the last case, see “Details”.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

Image data are on a regular grid in lon-lat space, but not in the
projected x-y space. This means that
[`image()`](https://rdrr.io/r/graphics/image.html) cannot be used.
Instead, there are two approaches, depending on the value of
`filledContour`.

If `filledContour` is `FALSE`, the image "pixels" are drawn with
[`polygon()`](https://rdrr.io/r/graphics/polygon.html). This can be
prohibitively slow for fine grids.

However, if `filledContour` is `TRUE`, then the "pixels" are remapped
into a regular grid and then displayed with
[`.filled.contour()`](https://rdrr.io/r/graphics/filled.contour.html).
The remapping starts by converting the regular lon-lat grid to an
irregular x-y grid using
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md).
This irregular grid is then interpolated onto a regular x-y grid in
accordance with the `gridder` parameter. If `gridder` values of
`"binMean2D"` and `"interp"` do not produce satisfactory results,
advanced users might wish to supply a function to do the gridding
according to their own criteria. The function must have as its first 5
arguments (1) an x vector, (2) a y vector, (3) a z matrix that
corresponds to x and y in the usual way, (4) a vector holding the
desired x grid, and (5) a vector holding the desired y grid. The return
value must be a list containing items named `xmids`, `ymids` and
`result`. To understand the meaning of the parameters and return values,
consult the documentation for
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md).
Here is an example of a scheme that will fill data gaps of 1 or 2 cells:

    g <- function(...) binMean2D(..., fill = TRUE, fillgap = 2)
    mapImage(..., gridder = g, ...)

## Historical Notes

Until oce 1.7.4, the `gridder` argument could be set to `"akima"`, which
used the `akima` package. However, that package is not released with a
FOSS license, so CRAN requested a change to
[interp](https://CRAN.R-project.org/package=interp). Note that
`drawImage()` intercepts the errors that sometimes get reported by
[`interp::interp()`](https://rdrr.io/pkg/interp/man/interp.html).

## Sample of Usage


    library(oce)
    data(coastlineWorld)
    data(topoWorld)

    # Northern polar region, with color-coded bathymetry
    par(mfrow = c(1, 1), mar = c(2, 2, 1, 1))
    cm <- colormap(zlim = c(-5000, 0), col = oceColorsGebco)
    drawPalette(colormap = cm)
    mapPlot(coastlineWorld,
        projection = "+proj=stere +lat_0=90",
        longitudelim = c(-180, 180), latitudelim = c(70, 110)
    )
    # Uncomment one of the next four blocks.  See
    # https://dankelley.github.io/dek_blog/2024/03/07/mapimage.html
    # for illustrations.

    # Method 1: the default, using polygons for lon-lat patches
    mapImage(topoWorld, colormap = cm)

    # Method 2: filled contours, with ugly missing-data traces
    # mapImage(topoWorld, colormap = cm, filledContour = TRUE)

    # Method 3: filled contours, with a double-sized grid cells
    # mapImage(topoWorld, colormap = cm, filledContour = 2)

    # Method 4: filled contours, with a gap-filling gridder)
    # g <- function(...) binMean2D(..., fill = TRUE, fillgap = 2)
    # mapImage(topoWorld, colormap = cm, filledContour = TRUE, gridder = g)

    mapGrid(15, 15, polarCircle = 1, col = gray(0.2))
    mapPolygon(coastlineWorld[["longitude"]],
        coastlineWorld[["latitude"]],
        col = "tan"
    )

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
