# Plot a gps Object

This function plots a gps object. An attempt is made to use the whole
space of the plot, and this is done by limiting either the longitude
range or the latitude range, as appropriate, by modifying the eastern or
northern limit, as appropriate. To get an inset map inside another map,
draw the first map, do `par(new=TRUE)`, and then call `plot.gps` with a
value of `mar` that moves the inset plot to a desired location on the
existing plot, and with `bg="white"`.

## Usage

``` r
# S4 method for class 'gps'
plot(
  x,
  xlab = "",
  ylab = "",
  asp,
  clongitude,
  clatitude,
  span,
  projection,
  expand = 1,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1, mgp[1] + 1, 1, 1),
  bg,
  axes = TRUE,
  cex.axis = par("cex.axis"),
  add = FALSE,
  inset = FALSE,
  geographical = 0,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a [gps](https://dankelley.github.io/oce/reference/gps-class.md)
  object.

- xlab:

  label for x axis

- ylab:

  label for y axis

- asp:

  Aspect ratio for plot. The default is for `plot.gps` to set the aspect
  ratio to give natural latitude-longitude scaling somewhere near the
  centre latitude on the plot. Often, it makes sense to set `asp`
  yourself, e.g. to get correct shapes at 45N, use
  `asp=1/cos(45*pi/180)`. Note that the land mass is not symmetric about
  the equator, so to get good world views you should set `asp=1` or set
  `ylim` to be symmetric about zero. Any given value of `asp` is
  ignored, if `clongitude` and `clatitude` are given.

- clongitude, clatitude:

  optional center latitude of map, in decimal degrees. If both
  `clongitude` and `clatitude` are provided, then any provided value of
  `asp` is ignored, and instead the plot aspect ratio is computed based
  on the center latitude. If `clongitude` and `clatitude` are provided,
  then `span` must also be provided.

- span:

  optional suggested span of plot, in kilometers. The suggestion is an
  upper limit on the scale; depending on the aspect ratio of the
  plotting device, the radius may be smaller than `span`. A value for
  `span` must be supplied, if `clongitude` and `clatitude` are supplied.

- projection:

  optional map projection to use (see
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md));
  if not given, a cartesian frame is used, scaled so that gps shapes
  near the centre of the plot are preserved. If a projection is
  provided, the coordinate system will bear an indirect relationship to
  longitude and longitude, and further adornment of the plot must be
  done with e.g.
  [`mapPoints()`](https://dankelley.github.io/oce/reference/mapPoints.md)
  instead of [`points()`](https://rdrr.io/r/graphics/points.html).

- expand:

  numerical factor for the expansion of plot limits, showing area
  outside the plot, e.g. if showing a ship track as a gps, and then an
  actual gps to show the ocean boundary. The value of `expand` is
  ignored if either `xlim` or `ylim` is given.

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- bg:

  optional color to be used for the background of the map. This comes in
  handy for drawing insets (see “details”).

- axes:

  boolean, set to `TRUE` to plot axes.

- cex.axis:

  value for axis font size factor.

- add:

  boolean, set to `TRUE` to draw the gps on an existing plot. Note that
  this retains the aspect ratio of that existing plot, so it is
  important to set that correctly, e.g. with
  `asp=1/cos(lat * pi / 180)`, where `clat` is the central latitude of
  the plot.

- inset:

  set to `TRUE` for use within
  [`plotInset()`](https://dankelley.github.io/oce/reference/plotInset.md).
  The effect is to prevent the present function from adjusting margins,
  which is necessary because margin adjustment is the basis for the
  method used by
  [`plotInset()`](https://dankelley.github.io/oce/reference/plotInset.md).

- geographical:

  flag indicating the style of axes. If `geographical=0`, the axes are
  conventional, with decimal degrees as the unit, and negative signs
  indicating the southern and western hemispheres. If `geographical=1`,
  the signs are dropped, with axis values being in decreasing order
  within the southern and western hemispheres. If `geographical=2`, the
  signs are dropped and the axes are labelled with degrees, minutes and
  seconds, as appropriate.

- debug:

  set to `TRUE` to get debugging information during processing.

- ...:

  optional arguments passed to plotting functions. For example, set
  `yaxp=c(-90,90,4)` for a plot extending from pole to pole.

## See also

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`plot,bremen-method`](https://dankelley.github.io/oce/reference/plot-bremen-method.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plot,ladp-method`](https://dankelley.github.io/oce/reference/plot-ladp-method.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`plot,lisst-method`](https://dankelley.github.io/oce/reference/plot-lisst-method.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`plot,satellite-method`](https://dankelley.github.io/oce/reference/plot-satellite-method.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to gps data:
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
`[[<-,gps-method`,
[`as.gps()`](https://dankelley.github.io/oce/reference/as.gps.md),
[`gps-class`](https://dankelley.github.io/oce/reference/gps-class.md),
[`read.gps()`](https://dankelley.github.io/oce/reference/read.gps.md),
[`summary,gps-method`](https://dankelley.github.io/oce/reference/summary-gps-method.md)

## Author

Dan Kelley
