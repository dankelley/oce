# Plot a tidem Object

Plot a summary diagram for a tidal fit.

## Usage

``` r
# S4 method for class 'tidem'
plot(
  x,
  which = 1,
  constituents = c("SA", "O1", "K1", "M2", "S2", "M4"),
  sides = NULL,
  col = "blue",
  log = "",
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1, mgp[1] + 1, mgp[2] + 0.25, mgp[2] + 1),
  ...
)
```

## Arguments

- x:

  a [tidem](https://dankelley.github.io/oce/reference/tidem-class.md)
  object.

- which:

  integer flag indicating plot type, 1 for stair-case spectral, 2 for
  spike spectral.

- constituents:

  character vector holding the names of constituents that are to be
  drawn and labelled. If `NULL`, then no constituents will be shown.

- sides:

  an integer vector of length equal to that of `constituents`,
  designating the side on which the constituent labels are to be drawn.
  As in all R graphics, the value `1` indicates the bottom of the plot,
  and `3` indicates the top. If `sides=NULL`, the default, then all
  labels are drawn at the top. Any value of `sides` that is not either 1
  or 3 is converted to 3.

- col:

  a character vector naming colors to be used for `constituents`.
  Ignored if `sides=3`. Repeated to be of the same length as
  `constituents`, otherwise.

- log:

  if set to "`x`", the frequency axis will be logarithmic.

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with `[`par`]("mar")`.

- ...:

  optional arguments passed to plotting functions, not all of which are
  obeyed. For example, if ... contains `type`, that value will be
  ignored because it is set internally, according to the value of
  `which`.

## Sample of Usage

    library(oce)
    data(sealevel)
    tide <- tidem(sealevel)
    plot(tide)

## Historical note

An argument named `labelIf` was removed in July 2016, because it was
discovered never to have worked as documented, and because the more
useful argument `constituents` had been added.

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
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley
