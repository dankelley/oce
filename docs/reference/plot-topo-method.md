# Plot a topo Object

This plots contours of topographic elevation. The plot aspect ratio is
set based on the middle latitude in the plot. The line properties, such
as `land.lwd`, may either be a single item, or a vector; in the latter
case, the length must match the length of the corresponding properties,
e.g. `land.z`.

## Usage

``` r
# S4 method for class 'topo'
plot(
  x,
  xlab = "",
  ylab = "",
  asp,
  clongitude,
  clatitude,
  span,
  expand = 1.5,
  water.z,
  col.water,
  lty.water,
  lwd.water,
  land.z,
  col.land,
  lty.land,
  lwd.land,
  geographical = FALSE,
  location = "topright",
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1, mgp[1] + 1, 1, 1),
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a [topo](https://dankelley.github.io/oce/reference/topo-class.md)
  object.

- xlab, ylab:

  Character strings giving a label for the x and y axes.

- asp:

  Aspect ratio for plot. The default is for `plot.coastline` to set the
  aspect ratio to give natural latitude-longitude scaling somewhere near
  the centre latitude on the plot. Often, it makes sense to set `asp`
  yourself, e.g. to get correct shapes at 45N, use
  `asp=1/cos(45*pi/180)`. Note that the land mass is not symmetric about
  the equator, so to get good world views you should set `asp=1` or set
  `ylim` to be symmetric about zero. Any given value of `asp` is
  ignored, if `clongitude` and `clatitude` are given.

- clongitude:

  Optional center longitude of map, in degrees east; see `clatitude`.

- clatitude:

  Optional center latitude of map, in degrees north. If this and
  `clongitude` are provided, then any provided value of `asp` is
  ignored, and instead the plot aspect ratio is computed based on the
  center latitude. Also, if `clongitude` and `clatitude` are provided,
  then `span` must be, also.

- span:

  Optional suggested span of plot, in kilometers (must be supplied, if
  `clongitude` and `clatitude` are supplied).

- expand:

  Numerical factor for the expansion of plot limits, showing area
  outside the plot, e.g. if showing a ship track as a coastline, and
  then an actual coastline to show the ocean boundary. The value of
  `expand` is ignored if either `xlim` or `ylim` is given.

- water.z:

  Depths at which to plot water contours. If not provided, these are
  inferred from the data.

- col.water:

  Colors corresponding to `water.z` values. If not provided, these will
  be `"fill"` colors from
  [`oce.colorsGebco()`](https://dankelley.github.io/oce/reference/oceColorsGebco.md).

- lty.water:

  Line type(s) for water contours.

- lwd.water:

  Line width(s) for water contours.

- land.z:

  Depths at which to plot land contours. If not provided, these are
  inferred from the data. If set to `NULL`, no land contours will be
  plotted.

- col.land:

  Colors corresponding to `land.z` values. If not provided, these will
  be `"fill"` colors from
  [`oce.colorsGebco()`](https://dankelley.github.io/oce/reference/oceColorsGebco.md).

- lty.land:

  Line type(s) for land contours.

- lwd.land:

  Line width(s) for land contours.

- geographical:

  Logical, indicating whether to plot latitudes and longitudes without
  minus signs.

- location:

  Location for a legend (or `"none"`, for no legend).

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  Four-element numerical vector to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- debug:

  Numerical value, with positive values indicating higher levels of
  debugging.

- ...:

  Additional arguments passed on to plotting functions.

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
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
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
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(topoWorld)
plot(topoWorld, clongitude = -60, clatitude = 45, span = 10000)

```
