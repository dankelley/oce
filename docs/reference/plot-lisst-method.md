# Plot a lisst Object

Creates a multi-panel summary plot of data measured by LISST instrument.

## Usage

``` r
# S4 method for class 'lisst'
plot(x, which = c(16, 37, 38), tformat, debug = getOption("oceDebug"), ...)
```

## Arguments

- x:

  a [lisst](https://dankelley.github.io/oce/reference/lisst-class.md)
  object.

- which:

  list of desired plot types. These are graphed in panels running down
  from the top of the page. See “Details” for the meanings of various
  values of `which`.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- debug:

  a flag that turns on debugging. The value indicates the depth within
  the call stack to which debugging applies.

- ...:

  optional arguments passed to plotting functions.

## Details

The panels are controlled by the `which` argument, as follows.

- `which=1` to `32`, or `which="C1"` to `"C32"` for a time-series graph
  of the named column (a size class).

- `which=33` or `which="lts"` for a time-series plot of laser
  transmission sensor.

- `which=34` or `which="voltage"` for a time-series plot of instrument
  voltage.

- `which=35` or `which="aux"` for a time-series plot of the external
  auxiliary input.

- `which=36` or `which="lrs"` for a time-series plot of the laser
  reference sensor.

- `which=37` or `which="pressure"` for a time-series plot of pressure.

- `which=38` or `which="temperature"` for a time-series plot of
  temperature.

- `which=41` or `which="transmission"` for a time-series plot of
  transmission, in percent.

- `which=42` or `which="beam"` for a time-series plot of beam-C, in
  1/metre.

## See also

The documentation for
[lisst](https://dankelley.github.io/oce/reference/lisst-class.md)
explains the structure of lisst objects, and also outlines the other
functions dealing with them.

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

Other things related to lisst data:
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
`[[<-,lisst-method`,
[`as.lisst()`](https://dankelley.github.io/oce/reference/as.lisst.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`read.lisst()`](https://dankelley.github.io/oce/reference/read.lisst.md),
[`summary,lisst-method`](https://dankelley.github.io/oce/reference/summary-lisst-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(lisst)
plot(lisst)

```
