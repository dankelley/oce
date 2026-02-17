# Plot an adv Object

Plot [adv](https://dankelley.github.io/oce/reference/adv-class.md) data.

## Usage

``` r
# S4 method for class 'adv'
plot(
  x,
  which = c(1:3, 14, 15),
  col,
  titles,
  type = "l",
  lwd = par("lwd"),
  drawTimeRange = getOption("oceDrawTimeRange"),
  drawZeroLine = FALSE,
  useSmoothScatter,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  tformat,
  marginsAsImage = FALSE,
  cex = par("cex"),
  cex.axis = par("cex.axis"),
  cex.lab = par("cex.lab"),
  cex.main = par("cex.main"),
  xlim,
  ylim,
  brushCorrelation,
  colBrush = "red",
  main = "",
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [adv](https://dankelley.github.io/oce/reference/adv-class.md)
  object.

- which:

  List of desired plot types. These are graphed in panels running down
  from the top of the page. See “Details” for the meanings of various
  values of `which`.

- col:

  Optional indication of color(s) to use. If not provided, the default
  for images is `oce.colorsPalette(128,1)`, and for lines and points is
  black.

- titles:

  Optional vector of character strings to be used as labels for the plot
  panels. For images, these strings will be placed in the right hand
  side of the top margin. For timeseries, these strings are ignored. If
  this is provided, its length must equal that of `which`.

- type:

  Type of plot, as for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- lwd:

  If the plot is of a time-series or scattergraph format with lines,
  this is used in the usual way; otherwise, e.g. for image formats, this
  is ignored.

- drawTimeRange:

  Logical value that applies to panels with time as the horizontal axis,
  indicating whether to draw the time range in the top-left margin of
  the plot.

- drawZeroLine:

  Logical value indicating whether to draw zero lines on velocities.

- useSmoothScatter:

  Logical value indicating whether to use
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html) in
  various plots, such as `which="uv"`. If not provided a default is
  used, with
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)
  being used if there are more than 2000 points to plot.

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  Value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- tformat:

  Optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- marginsAsImage:

  Logical value indicating whether to put a wide margin to the right of
  time-series plots, matching the space used up by a palette in an
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md)
  plot.

- cex:

  numeric character expansion factor for plot symbols; see
  [`par()`](https://rdrr.io/r/graphics/par.html).

- cex.axis, cex.lab, cex.main:

  character expansion factors for axis numbers, axis names and plot
  titles; see [`par()`](https://rdrr.io/r/graphics/par.html).

- xlim:

  Optional 2-element list for `xlim`, or 2-column matrix, in which case
  the rows are used, in order, for the panels of the graph.

- ylim:

  Optional 2-element list for `ylim`, or 2-column matrix, in which case
  the rows are used, in order, for the panels of the graph.

- brushCorrelation:

  Optional number between 0 and 100, indicating a per-beam correlation
  threshold below which data are to be considered suspect. If the plot
  type is `p`, the suspect points (velocity, backscatter amplitude, or
  correlation) will be colored red; otherwise, this argument is ignored.

- colBrush:

  Color to use for brushed (bad) data, if `brushCorrelation` is active.

- main:

  Main title for plot, used just on the top panel, if there are several
  panels.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  Optional arguments passed to plotting functions.

## Details

Creates a multi-panel summary plot of data measured by an ADV. The
panels are controlled by the `which` argument. (Note the gaps in the
sequence, e.g. 4 and 8 are not used.)

- `which=1` to `3` (or `"u1"` to `"u3"`) yield timeseries of the first,
  second, and third components of velocity (in beam, xyz or enu
  coordinates).

- `which=4` is not permitted (since ADV are 3-beam devices)

- `which=5` to `7` (or `"a1"` to `"a3"`) yield timeseries of the
  amplitudes of beams 1 to 3. (Note that the data are called
  `data$a[,1]`, `data$a[,2]` and `data$a[,3]`, for these three
  timeseries.)

- `which=8` is not permitted (since ADV are 3-beam devices)

- `which=9` to `11` (or `"q1"` to `"q3"`) yield timeseries of
  correlation for beams 1 to 3. (Note that the data are called
  `data$c[,1]`, `data$c[,2]` and `data$c[,3]`, for these three
  timeseries.)

- `which=12` is not permitted (since ADVs are 3-beam devices)

- `which=13` is not permitted (since ADVs do not measure salinity)

- `which=14` or `which="temperature"` yields a timeseries of
  temperature.

- `which=15` or `which="pressure"` yields a timeseries of pressure.

- `which=16` or `which="heading"` yields a timeseries of heading.

- `which=17` or `which="pitch"`yields a timeseries of pitch.

- `which=18` or `which="roll"`yields a timeseries of roll.

- `which=19` to `21` yields plots of correlation versus amplitude, for
  beams 1 through 3, using
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html).

- `which=22` is not permitted (since ADVs are 3-beam devices)

- `which=23` or `"progressive vector"` yields a progressive-vector
  diagram in the horizontal plane, plotted with `asp=1`, and taking
  beam1 and beam2 as the eastward and northward components of velocity,
  respectively.

- `which=28` or `"uv"` yields velocity plot in the horizontal plane,
  i.e. `u[2]` versus `u[1]`. If the number of data points is small, a
  scattergraph is used, but if it is large,
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html) is
  used.

- `which=29` or `"uv+ellipse"` as the `"uv"` case, but with an added
  indication of the tidal ellipse, calculated from the eigen vectors of
  the covariance matrix.

- `which=30` or `"uv+ellipse+arrow"` as the `"uv+ellipse"` case, but
  with an added arrow indicating the mean current.

- `which=50` or `"analog1"` plots a time series of the analog1 signal,
  if there is one.

- `which=51` or `"analog2"` plots a time series of the analog2 signal,
  if there is one.

- `which=100` or `"voltage"` plots the voltage as a timeseries, if
  voltage exists in the dataset.

In addition to the above, there are some groupings defined:

- `which="velocity"` equivalent to `which=1:3` (three velocity
  components)

- `which="amplitude"` equivalent to `which=5:7` (three amplitude
  components)

- `which="backscatter"` equivalent to `which=9:11` (three backscatter
  components)

- `which="hydrography"` equivalent to `which=14:15` (temperature and
  pressure)

- `which="angles"` equivalent to `which=16:18` (heading, pitch and roll)

## See also

The documentation for
[adv](https://dankelley.github.io/oce/reference/adv-class.md) explains
the structure of ADV objects, and also outlines the other functions
dealing with them.

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
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
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdv()`](https://dankelley.github.io/oce/reference/enuToOtherAdv.md),
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
[`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md),
[`read.adv.sontek.adr()`](https://dankelley.github.io/oce/reference/read.adv.sontek.adr.md),
[`read.adv.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adv.sontek.serial.md),
[`read.adv.sontek.text()`](https://dankelley.github.io/oce/reference/read.adv.sontek.text.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`summary,adv-method`](https://dankelley.github.io/oce/reference/summary-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(adv)
plot(adv)

```
