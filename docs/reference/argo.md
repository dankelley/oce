# Sample argo Data

This holds data from ARGO 6900388 in the North Atlantic.

## Source

The NetCDF file used by
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
to create this
[argo](https://dankelley.github.io/oce/reference/argo-class.md) object
was downloaded using FTP to
`ftp.ifremer.fr/ifremer/argo/dac/bodc/6900388/6900388_prof.nc` on 2020
June 24.

## Details

Below is the official citation (note that this DOI has web links for
downloads):

Argo (2017). Argo float data and metadata from Global Data Assembly
Centre (Argo GDAC) - Snapshot of Argo GDAC of July, 8st 2017. SEANOE.
DOI:`10.17882/42182#50865`

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
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
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Examples

``` r
library(oce)
data(argo)
summary(argo)
#> Argo Summary
#> ------------
#> 
#> * Source:              "/Users/kelley/git/oce/create_data/argo/6900388_prof.nc"
#> * ID:                  "6900388"
#> * Feature type:        "trajectoryProfile"
#> * Profiles:            210 delayed; 0 adjusted; 13 realtime
#> * Time: 2005-10-29 13:57:42 to 2011-11-27 17:58:39 (223 samples, mean increment 10.00075 day)
#> * Data Overview
#> 
#>                                           Min.       Mean       Max.       Dim.     NAs OriginalName         
#>     time                                  1130594262 1226506685 1322416720 223      0   "-"                  
#>     latitude [°N]                         48.743     56.715     64.335     223      0   "LATITUDE"           
#>     longitude [°E]                        -60.52     -37.077    -21.385    223      0   "LONGITUDE"          
#>     pressure [dbar]                       3.5        521.31     6534.6     "56x223" 106 "PRES"               
#>     pressureAdjusted [dbar]               3.6        520.34     1778.9     "56x223" 874 "PRES_ADJUSTED"      
#>     pressureAdjustedError [dbar]          2.4        2.4        2.4        "56x223" 874 "PRES_ADJUSTED_ERROR"
#>     salinity [PSS-78]                     0          34.911     47.899     "56x223" 106 "PSAL"               
#>     salinityAdjusted [PSS-78]             32.85      34.935     35.842     "56x223" 840 "PSAL_ADJUSTED"      
#>     salinityAdjustedError [PSS-78]        0.01       0.01       0.01       "56x223" 840 "PSAL_ADJUSTED_ERROR"
#>     temperature [°C, ITS-90]              -1.564     6.1216     55.997     "56x223" 106 "TEMP"               
#>     temperatureAdjusted [°C, ITS-90]      -1.564     6.0916     14.914     "56x223" 837 "TEMP_ADJUSTED"      
#>     temperatureAdjustedError [°C, ITS-90] 0.002      0.002      0.002      "56x223" 837 "TEMP_ADJUSTED_ERROR"
#> 
#> * Data-quality Flag Scheme
#> 
#>     name    "argo"
#>     mapping list(not_assessed=0, passed_all_tests=1, probably_good=2, probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7, estimated=8, missing=9)
#>     default c(0, 3, 4, 9)
#> 
#> * Data-quality Flags
#> 
#>     pressure:            "1" 12327, "4" 55
#>     pressureAdjusted:    "2" 11614
#>     salinity:            "1" 12348, "4" 34
#>     salinityAdjusted:    "2" 11648
#>     temperature:         "1" 12362, "4" 20
#>     temperatureAdjusted: "2" 11651
#> 
#> * Processing Log
#> 
#>     - 2022-04-11 15:55:43 UTC: `create 'argo' object`
#>     - 2022-04-11 15:55:43 UTC: `initializeFlagScheme(object, name="argo", mapping=list(not_assessed=0,passed_all_tests=1,probably_good=2,probably_bad=3,bad=4,changed=5,not_used_6=6,not_used_7=7,estimated=8,missing=9)), default=c(0,3,4,9))`
#>     - 2022-04-11 15:55:44 UTC: `read.argo(file="/Users/kelley/git/oce/create_data/argo/6900388_prof.nc")`
data(coastlineWorld)
plot(argo, which = "trajectory")

```
