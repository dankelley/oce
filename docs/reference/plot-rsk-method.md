# Plot a rsk Object

Rsk data may be in many forms, and it is not easy to devise a general
plotting strategy for all of them. The present function is quite crude,
on the assumption that users will understand their own datasets, and
that they can devise plots that are best-suited to their applications.
Sometimes, the sensible scheme is to coerce the object into another
form, e.g. using `plot(as.ctd(rsk))` if the object contains CTD-like
data. Other times, users should extract data from the `rsk` object and
construct plots themselves. The idea is to use the present function
mainly to get an overview, and for that reason, the default plot type
(set by `which`) is a set of time-series plots, because the one thing
that is definitely known about `rsk` objects is that they contain a
`time` vector in their `data` slot.

## Usage

``` r
# S4 method for class 'rsk'
plot(
  x,
  which = "timeseries",
  tlim,
  ylim,
  xlab,
  ylab,
  tformat,
  drawTimeRange = getOption("oceDrawTimeRange"),
  abbreviateTimeRange = getOption("oceAbbreviateTimeRange"),
  useSmoothScatter = FALSE,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  main = "",
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
  object.

- which:

  character indicating desired plot types. These are graphed in panels
  running down from the top of the page. See “Details” for the meanings
  of various values of `which`.

- tlim:

  optional limits for time axis. If not provided, the value will be
  inferred from the data.

- ylim:

  optional limits for the y axis. If not provided, the value will be
  inferred from the data. (It is helpful to specify this, if the
  auto-scaled value will be inappropriate, e.g. if more lines are to be
  added later). Note that this is ignored, unless `length(which) == 1`
  and `which` corresponds to one of the data fields. If a multipanel
  plot of a specific subset of the data fields is desired with `ylim`
  control, it should be done panel by panel (see Examples).

- xlab:

  optional label for x axis.

- ylab:

  optional label for y axis.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- drawTimeRange:

  boolean that applies to panels with time as the horizontal axis,
  indicating whether to draw the time range in the top-left margin of
  the plot.

- abbreviateTimeRange:

  boolean that applies to panels with time as the horizontal axis,
  indicating whether to abbreviate the second time in the time range
  (e.g. skipping the year, month, day, etc. if it's the same as the
  start time).

- useSmoothScatter:

  a boolean to cause
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html) to
  be used for profile plots, instead of
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- mgp:

  3-element numerical vector to use for
  [`par`](https://rdrr.io/r/graphics/par.html)`("mgp")`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- main:

  main title for plot, used just on the top panel, if there are several
  panels.

- debug:

  a flag that turns on debugging, if it exceeds 0.

- ...:

  optional arguments passed to plotting functions.

## Details

Plots produced are time series plots of the data in the object. The
default, `which="timeseries"` plots all data fields, and over-rides any
other specification. Specific fields can be plotted by naming the field,
e.g. `which="temperature"` to plot a time series of just the temperature
field.

## See also

The documentation for
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) explains
the structure of `rsk` objects, and also outlines the other functions
dealing with them.

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

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

## Author

Dan Kelley and Clark Richards

## Examples

``` r
library(oce)
data(rsk)
# 1. default timeseries plot of all data fields
plot(rsk)

# 2. plot in ctd format
plot(as.ctd(rsk))

```
