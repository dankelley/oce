# Sample xbt File in space-separated .edf Format

Sample xbt File in space-separated .edf Format

## See also

Other raw datasets:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

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
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

## Examples

``` r
xbt <- read.xbt(system.file("extdata", "xbt.edf", package = "oce"))
summary(xbt)
#> xbt summary
#> -----------
#> 
#> * File source:        "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpWZrgRz/temp_libpath1549d10bb9024/oce/extdata/xbt.edf"
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
#>     - 2026-02-17 19:26:40 UTC: `create 'xbt' object`
#>     - 2026-02-17 19:26:40 UTC: `read.xbt.edf(file = file, longitude = longitude, latitude = latitude,     encoding = encoding, debug = debug - 1L, processingLog = processingLog)`
```
