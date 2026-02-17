# Sample xbt Data

An [xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object
created by using
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md) on
a Sippican file created by extracting the near-surface fraction of the
sample provided in Section 5.5.6 of reference 1.

## Usage

``` r
data(xbt)
```

## References

1.  Sippican, Inc. "Bathythermograph Data Acquisition System:
    Installation, Operation and Maintenance Manual (P/N 308195, Rev.
    A)," 2003.
    https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.

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
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md)

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

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
