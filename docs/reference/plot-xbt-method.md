# Plot an xbt Object

Plots data contained in an
[xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object.

## Usage

``` r
# S4 method for class 'xbt'
plot(
  x,
  which = 1,
  type = "l",
  mgp = getOption("oceMgp"),
  mar,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [xbt](https://dankelley.github.io/oce/reference/xbt-class.md)
  object.

- which:

  list of desired plot types; see “Details” for the meanings of various
  values of `which`.

- type:

  type of plot, as for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  optional arguments passed to plotting functions.

## Details

The panels are controlled by the `which` argument, with choices as
follows.

- `which=1` for a temperature profile as a function of depth.

- `which=2` for a soundSpeed profile as a function of depth.

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
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(xbt)
summary(xbt)
#> xbt summary
#> -----------
#> 
#> * File source:        "/Users/kelley/git/oce/create_data/xbt/xbt.edf"
#> * Serial Number:      0
#> * Longitude:          4.005
#> * Latitude:           -4
#> * Time:               2000-10-10 08:49:38
#> * Data Overview
#> 
#>                              Min.   Mean   Max.   Dim. NAs OriginalName    
#>     depth [m]                5.4    7.0333 8.7    6    0   "Depth"         
#>     temperature [°C, ITS-90] 20.9   20.905 20.91  6    0   "Temperature"   
#>     soundSpeed [m/s]         1575.3 1575.3 1575.4 6    0   "Sound Velocity"
#> 
#> * Processing Log
#> 
#>     - 2020-02-24 15:07:55 UTC: `create 'xbt' object`
#>     - 2020-02-24 15:07:55 UTC: `read.xbt.edf(file = file, debug = 10)`
plot(xbt)

```
