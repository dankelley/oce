# Plot a sealevel Object

Creates a plot for a sea-level dataset, in one of two varieties.
Depending on the length of `which`, either a single-panel or multi-panel
plot is drawn. If there is just one panel, then the value of `par` used
in `plot,sealevel-method` is retained upon exit, making it convenient to
add to the plot. For multi-panel plots, `par` is returned to the value
it had before the call.

## Usage

``` r
# S4 method for class 'sealevel'
plot(
  x,
  which = 1:3,
  drawTimeRange = getOption("oceDrawTimeRange"),
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 0.5, mgp[1] + 1.5, mgp[2] + 1, mgp[2] + 3/4),
  marginsAsImage = FALSE,
  grid = TRUE,
  xlim,
  ylim,
  xaxs = "i",
  yaxs = "r",
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a
  [sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
  object.

- which:

  a numerical or string vector indicating desired plot types, with
  possibilities 1 or `"all"` for a time-series of all the elevations, 2
  or `"month"` for a time-series of just the first month, 3 or
  `"spectrum"` for a power spectrum (truncated to frequencies below 0.1
  cycles per hour, or 4 or `"cumulativespectrum"` for a cumulative
  integral of the power spectrum.

- drawTimeRange:

  boolean that applies to panels with time as the horizontal axis,
  indicating whether to draw the time range in the top-left margin of
  the plot.

- mgp:

  3-element numerical vector to use for
  [`par`](https://rdrr.io/r/graphics/par.html)`("mgp")`, and also for
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`, computed from
  this. The default is tighter than the R default, in order to use more
  space for the data and less for the axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- marginsAsImage:

  logical value indicating whether to put a wide margin to the right of
  time-series plots, matching the space used up by a palette in an
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md)
  plot.

- grid:

  logical value, indicating whether to draw a grid with
  [`grid()`](https://rdrr.io/r/graphics/grid.html).

- xlim, ylim:

  optional limits for axes. If not supplied, reasonable choices will be
  made

- xaxs, yaxs:

  axis-limit parameters, as for standard graphics. The default is to
  make the time axis extend to the edges of the box, but to make the y
  axis have some space above and below the range of the data.

- debug:

  a flag that turns on debugging, if it exceeds 0.

- ...:

  optional arguments passed to plotting functions.

## Value

None.

## Historical Note

Until 2020-02-06, sea-level plots had the mean value removed, and
indicated with a tick mark and margin note on the right-hand side of the
plot. This behaviour was confusing. The change did not go through the
usual deprecation process, because the margin-note behaviour had not
been documented.

## References

The example refers to Hurricane Juan, which caused a great deal of
damage to Halifax in 2003. Since this was in the era of the digital
photo, a casual web search will uncover some spectacular images of
damage, from both wind and storm surge. Landfall, within 30km of this
sealevel gauge, was between 00:10 and 00:20 Halifax local time on
Monday, Sept 29, 2003.

## See also

The documentation for the
[sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
class explains the structure of sealevel objects, and also outlines the
other functions dealing with them.

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
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`read.sealevel.gc2026()`](https://dankelley.github.io/oce/reference/read.sealevel.gc2026.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(sealevel)
# local Halifax time is UTC + 4h
juan <- as.POSIXct("2003-09-29 00:15:00", tz = "UTC") + 4 * 3600
plot(sealevel, which = 1, xlim = juan + 86400 * c(-7, 7))
abline(v = juan, col = "red")

```
