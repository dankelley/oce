# Plot a cm Object

Creates a multi-panel summary plot of data measured by a current meter.

## Usage

``` r
# S4 method for class 'cm'
plot(
  x,
  which = c(1:2),
  type = "l",
  xlim,
  ylim,
  xaxs = "r",
  yaxs = "r",
  drawTimeRange = getOption("oceDrawTimeRange"),
  drawZeroLine = FALSE,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  small = 2000,
  main = "",
  tformat,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a [cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

- which:

  list of desired plot types. These are graphed in panels running down
  from the top of the page. See “Details” for the meanings of various
  values of `which`.

- type:

  type of plot, as for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- xlim, ylim:

  optional limit to the x and y axes, passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  for time-series plots.

- xaxs, yaxs:

  optional controls over the limits of the x and y axes, passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
  for time-series plots. These values default to `"r"`, meaning to use
  the regular method of extend the plot past its normal limits. It is
  common to use `"i"` to make the graph extend to the panel limits.

- drawTimeRange:

  boolean that applies to panels with time as the horizontal axis,
  indicating whether to draw the time range in the top-left margin of
  the plot.

- drawZeroLine:

  boolean that indicates whether to draw zero lines on velocities.

- mgp:

  3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- small:

  an integer indicating the size of data set to be considered "small",
  to be plotted with points or lines using the standard
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function.
  Data sets with more than `small` points will be plotted with
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)
  instead.

- main:

  main title for plot, used just on the top panel, if there are several
  panels.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  Optional arguments passed to plotting functions.

## Details

The panels are controlled by the `which` argument, as follows.

- `which=1` or `which="u"` for a time-series graph of eastward velocity,
  `u`, as a function of time.

- `which=2` or `which="v"` for a time-series graph of northward
  velocity, `u`, as a function of time.

- `which=3` or `"progressive vector"` for progressive-vector plot

- `which=4` or `"uv"` for a plot of `v` versus `u`. (Dots are used for
  small datasets, and smoothScatter for large ones.)

- `which=5` or `"uv+ellipse"` as the `"uv"` case, but with an added
  indication of the tidal ellipse, calculated from the eigen vectors of
  the covariance matrix.

- `which=6` or `"uv+ellipse+arrow"` as the `"uv+ellipse"` case, but with
  an added arrow indicating the mean current.

- `which=7` or `"pressure"` for pressure

- `which=8` or `"salinity"` for salinity

- `which=9` or `"temperature"` for temperature

- `which=10` or `"TS"` for a TS diagram

- `which=11` or `"conductivity"` for conductivity

- `which=20` or `"direction"` for the direction of flow

## See also

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`plot,bremen-method`](https://dankelley.github.io/oce/reference/plot-bremen-method.md),
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

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(cm)
summary(cm)
#> Cm summary
#> ----------
#> 
#> * File source:   "/Users/kelley/Dropbox/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab"
#> * Instr. type:   (2Hz)S4
#> * Serial Num.:   8111786
#> * Version:       2.399
#> * North:         magnetic
#> * Time: 2008-06-26 to 2008-06-26 23:59:00 (1440 samples, mean increment 1 min)
#> * Data Overview
#> 
#>                              Min.       Mean       Max.       Dim. NAs  OriginalName
#>     Sample..                 842        1561.5     2281       1440 0    "Sample #"  
#>     Date                     NA         NA         NA         1440 0    "Date"      
#>     Time                     NA         NA         NA         1440 0    "Time"      
#>     dec.S                    0          0          0          1440 0    "dec S"     
#>     v [m/s]                  -0.862     -0.088736  0.74       1440 0    "Vnorth"    
#>     u [m/s]                  -0.478     0.02761    0.654      1440 0    "Veast"     
#>     Speed                    1.44       53.569     98.29      1440 0    "Speed"     
#>     Dir                      7          126.99     352.1      1440 0    "Dir"       
#>     Vref                     NA         NA         NA         1440 1440 "Vref"      
#>     Hx                       -210       -31.052    177        1440 0    "Hx"        
#>     Hy                       -212       -0.58889   211        1440 0    "Hy"        
#>     conductivity [mS/cm]     36.8       36.924     37.2       1440 0    "Cond"      
#>     temperature [°C, ITS-90] 1.204      2.0773     3.065      1440 0    "T-Temp"    
#>     Depth                    73.242     74.767     77.148     1440 0    "Depth"     
#>     none                     NA         NA         NA         1440 1440 "none"      
#>     none.1                   NA         NA         NA         1440 1440 "none"      
#>     Hdg                      0          175.79     359.2      1440 0    "Hdg"       
#>     X                        NA         NA         NA         1440 1440 "-"         
#>     salinity [PSS-78]        41.307     42.71      44.205     1440 0    "Sal"       
#>     Dens                     1033.3     1034.5     1035.8     1440 0    "Dens"      
#>     SV                       1467.9     1469.8     1472.2     1440 0    "SV"        
#>     X.1                      NA         NA         NA         1440 1440 "-"         
#>     N.S.Dist                 -2535.2    4159.5     10349      1440 0    "N-S Dist"  
#>     E.W.Dist                 -2033.5    893.81     4687.5     1440 0    "E-W Dist"  
#>     X.2                      NA         NA         NA         1440 1440 "-"         
#>     SRB.Date                 NA         NA         NA         1440 1440 "SRB Date"  
#>     SRB.Time                 NA         NA         NA         1440 0    "SRB Time"  
#>     Vref.1                   1.226      1.2263     1.23       1440 1152 "Vref"      
#>     Hx.1                     NA         NA         NA         1440 1440 "Hx"        
#>     Hy.1                     NA         NA         NA         1440 1440 "Hy"        
#>     Cond.1                   NA         NA         NA         1440 1440 "Cond"      
#>     T.Temp.1                 NA         NA         NA         1440 1440 "T-Temp"    
#>     Depth.1                  NA         NA         NA         1440 1440 "Depth"     
#>     none.2                   NA         NA         NA         1440 1440 "none"      
#>     none.3                   NA         NA         NA         1440 1440 "none"      
#>     Hdg.1                    NA         NA         NA         1440 1440 "Hdg"       
#>     X.3                      NA         NA         NA         1440 1440 "-"         
#>     Sal.1                    NA         NA         NA         1440 1440 "Sal"       
#>     Dens.1                   NA         NA         NA         1440 1440 "Dens"      
#>     SV.1                     NA         NA         NA         1440 1440 "SV"        
#>     pressure [dbar]          73.855     75.392     77.794     1440 0    "-"         
#>     time                     1214438400 1214481570 1214524740 1440 0    "-"         
#> 
#> * Processing Log
#> 
#>     - 2023-02-10 12:50:57 UTC: `create 'cm' object`
#>     - 2023-02-10 12:50:57 UTC: `read.oce("~/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab", ...)`
#>     - 2023-02-10 12:50:57 UTC: `subset.cm(x, subset=time < as.POSIXct("2008-06-27 00:00:00", tz = "UTC"))`
plot(cm)

```
