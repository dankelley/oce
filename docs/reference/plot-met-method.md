# Plot a met Object

Creates a multi-panel summary plot of data measured in a meteorological
data set. cast. The panels are controlled by the `which` argument.

## Usage

``` r
# S4 method for class 'met'
plot(x, which = 1:4, mgp, mar, tformat, debug = getOption("oceDebug"))
```

## Arguments

- x:

  a [met](https://dankelley.github.io/oce/reference/met-class.md)
  object.

- which:

  list of desired plot types.

  - `which=1` gives a time-series plot of temperature

  - `which=2` gives a time-series plot of pressure

  - `which=3` gives a time-series plot of the x (eastward) component of
    velocity

  - `which=4` gives a time-series plot of the y (northward) component of
    velocity

  - `which=5` gives a time-series plot of speed

  - `which=6` gives a time-series plot of direction (degrees clockwise
    from north; note that the values returned by `met[["direction"]]`
    must be multiplied by 10 to get the direction plotted)

- mgp:

  A 3-element numerical vector used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mgp")` to control the
  spacing of axis elements. The default is tighter than the R default.

- mar:

  A 4-element numerical vector used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")` to control the
  plot margins. The default is tighter than the R default.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

If more than one panel is drawn, then on exit from `plot.met`, the value
of `par` will be reset to the value it had before the function call.
However, if only one panel is drawn, the adjustments to `par` made
within `plot.met` are left in place, so that further additions may be
made to the plot.

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

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(met)
plot(met, which = 3:4)


# Wind speed and direction during Hurricane Juan
# Compare with the final figure in a white paper by Chris Fogarty
# (available at http://www.novaweather.net/Hurricane_Juan_files/McNabs_plot.pdf
# downloaded 2017-01-02).
library(oce)
data(met)
t0 <- as.POSIXct("2003-09-29 04:00:00", tz = "UTC")
dt <- 12 * 3600
juan <- subset(met, t0 - dt <= time & time <= t0 + dt)
par(mfrow = c(2, 1))
plot(juan, which = 5)
abline(v = t0)
plot(juan, which = 6)
abline(v = t0)

```
