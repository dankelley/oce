# Sample xbt File in tab-separated .edf Format

Sample xbt File in tab-separated .edf Format

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
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md)

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
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md)

## Examples

``` r
xbt2 <- read.xbt(system.file("extdata", "xbt2.edf", package = "oce"),
    type = "sippican2"
)
summary(xbt2)
#> xbt summary
#> -----------
#> 
#> * File source:        "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpWZrgRz/temp_libpath1549d10bb9024/oce/extdata/xbt2.edf"
#> * Instrument type:    T-xx
#> * Serial Number:      1
#> * Longitude:          -50.5
#> * Latitude:           40.5
#> * Time:               2025-01-07 12:01:02
#> * Data Overview
#> 
#>                      Min. Mean   Max. Dim. NAs OriginalName    
#>     time [s]         0    0.05   0.1  2    0   "Time"          
#>     resistance [ohm] 6600 6600.5 6601 2    0   "Resistance"    
#>     depth [m]        0    0.25   0.5  2    0   "Depth"         
#>     temperature [°C] 17.9 17.95  18   2    0   "Temperature"   
#>     soundSpeed [m/s] 1520 1520.5 1521 2    0   "Sound Velocity"
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 19:26:41 UTC: `create 'xbt' object`
```
