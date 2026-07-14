# Sample lobo Data

This is sample lobo dataset obtained in the Northwest Arm of Halifax by
Satlantic.

## Source

The data were downloaded from a web interface at Satlantic LOBO web
server and then read with
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md).

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`as.lobo()`](https://dankelley.github.io/oce/reference/as.lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`summary,lobo-method`](https://dankelley.github.io/oce/reference/summary-lobo-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(lobo)
summary(lobo)
#> Lobo Summary
#> ------------
#> 
#> * source: "/Users/kelley/src/oce/create_data/lobo/lobo.dat"
#> * Time: 2009-03-01 to 2009-04-01 23:00:00 (768 samples, mean increment 1 hour)
#> * Data Overview
#> 
#>                                 Min.  Mean     Max. Dim. NAs
#>     time                        NA    NA       NA   768  0  
#>     u [m/s]                     NA    NA       NA   768  768
#>     v [m/s]                     NA    NA       NA   768  768
#>     salinity [PSS-78]           13.61 29.208   30.8 768  0  
#>     temperature [°C, ITS-90]    -0.72 1.691    4.55 768  0  
#>     airtemperature [°C, ITS-90] -10.4 -0.20573 11.8 768  0  
#>     pressure [dbar]             NA    NA       NA   768  768
#>     nitrate [μM]                0.3   6.7108   38.5 768  0  
#>     fluorescence [μg/l]         0.32  1.2463   4.83 768  0  
#> 
#> * Processing Log
#> 
#>     - 2016-01-10 15:34:18 UTC: `create 'lobo' object`
#>     - 2016-01-10 15:34:18 UTC: `read.lobo(file = "lobo.dat")`
#>     - 2016-01-10 15:34:19 UTC: `subset.lobo(x, subset=start <= time & time <= end)`
#>     - 2016-01-10 15:34:19 UTC: `subset.lobo(x, subset=!is.na(temperature))`
plot(lobo)
#> Warning: no good pressures, so setting all to 0.0 dbar

```
