# Plot a windrose Object

Plot a
[windrose](https://dankelley.github.io/oce/reference/windrose-class.md)
object.

## Usage

``` r
# S4 method for class 'windrose'
plot(
  x,
  type = c("count", "mean", "median", "fivenum"),
  convention = c("meteorological", "oceanographic"),
  mgp = getOption("oceMgp"),
  mar = c(mgp[1], mgp[1], 1 + mgp[1], mgp[1]),
  col,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  a
  [windrose](https://dankelley.github.io/oce/reference/windrose-class.md)
  object.

- type:

  The thing to be plotted, either the number of counts in the angle
  interval, the mean of the values in the interval, the median of the
  values, or a [`fivenum()`](https://rdrr.io/r/stats/fivenum.html)
  representation of the values.

- convention:

  String indicating whether to use meteorological convention or
  oceanographic convention for the arrows that emanate from the centre
  of the rose. In meteorological convection, an arrow emanates towards
  the right on the diagram if the wind is from the east; in
  oceanographic convention, such an arrow indicates flow *to* the east.

- mgp:

  Three-element numerical vector to use for
  [`par`](https://rdrr.io/r/graphics/par.html)`(mgp)`, and also for
  [`par`](https://rdrr.io/r/graphics/par.html)`(mar)`, computed from
  this. The default is tighter than the R default, in order to use more
  space for the data and less for the axes.

- mar:

  Four-element numerical vector to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- col:

  Optional list of colors to use. If not set, the colors will be
  `c("red", "pink", "blue", "lightgray")`. For the first three types of
  plot, the first color in this list is used to fill in the rose, the
  third is used for the petals of the rose, and the fourth is used for
  grid lines. For the `"fivenum"` type, the region from the lower hinge
  to the first quartile is coloured pink, the region from the first
  quartile to the third quartile is coloured red, and the region from
  the third quartile to the upper hinge is coloured pink. Then the
  median is drawn in black.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

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
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to windrose data:
[`[[,windrose-method`](https://dankelley.github.io/oce/reference/sub-sub-windrose-method.md),
`[[<-,windrose-method`,
[`as.windrose()`](https://dankelley.github.io/oce/reference/as.windrose.md),
[`summary,windrose-method`](https://dankelley.github.io/oce/reference/summary-windrose-method.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
set.seed(1234)
theta <- seq(0, 360, 0.25)
x <- 1 + cos(pi / 180 * theta) + rnorm(theta)
y <- sin(pi / 180 * theta) + rnorm(theta)
wr <- as.windrose(x, y)
plot(wr)

plot(wr, type = "fivenum")

```
