# Plot a lobo object

Plot a summary diagram for lobo data.

## Usage

``` r
# S4 method for class 'lobo'
plot(
  x,
  which = c(1, 2, 3),
  mgp = getOption("oceMgp"),
  mar = c(mgp[2] + 1, mgp[1] + 1, 1, mgp[1] + 1.25),
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
  object.

- which:

  A vector of numbers or character strings, indicating the quantities to
  plot. These are stacked in a single column. The possible values for
  `which` are as follows: `1` or `"temperature"` for a time series of
  temperature; `2` or `"salinity"` for salinity; `3` or `"TS"` for a TS
  diagram (which uses `eos="unesco"`), `4` or `"u"` for a timeseries of
  the u component of velocity; `5` or `"v"` for a timeseries of the v
  component of velocity; `6` or `"nitrate"` for a timeseries of nitrate
  concentration; `7` or `"fluorescence"` for a timeseries of
  fluorescence value.

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  optional arguments passed to plotting functions.

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

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`as.lobo()`](https://dankelley.github.io/oce/reference/as.lobo.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`summary,lobo-method`](https://dankelley.github.io/oce/reference/summary-lobo-method.md)

## Author

Dan Kelley
