# Sample ctd File in aml Format

This file may be read with
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md).
It is based on a file donated by Ashley Stanek, which was shortened to
just 50 points for inclusion in oce, and which had some identifying
information (serial number, IP address, and WEP code) zeroed-out.

## See also

Other raw datasets:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md)

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`as.ctd`](https://dankelley.github.io/oce/reference/as.ctd.md)`()`,
[`cnvName2oceName`](https://dankelley.github.io/oce/reference/cnvName2oceName.md)`()`,
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctdDecimate`](https://dankelley.github.io/oce/reference/ctdDecimate.md)`()`,
[`ctdFindProfiles`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md)`()`,
[`ctdFindProfilesRBR`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md)`()`,
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`ctdRepair`](https://dankelley.github.io/oce/reference/ctdRepair.md)`()`,
[`ctdTrim`](https://dankelley.github.io/oce/reference/ctdTrim.md)`()`,
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`oceNames2whpNames`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md)`()`,
[`oceUnits2whpUnits`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md)`()`,
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plotProfile`](https://dankelley.github.io/oce/reference/plotProfile.md)`()`,
[`plotScan`](https://dankelley.github.io/oce/reference/plotScan.md)`()`,
[`plotTS`](https://dankelley.github.io/oce/reference/plotTS.md)`()`,
[`read.ctd`](https://dankelley.github.io/oce/reference/read.ctd.md)`()`,
[`read.ctd.aml`](https://dankelley.github.io/oce/reference/read.ctd.aml.md)`()`,
[`read.ctd.itp`](https://dankelley.github.io/oce/reference/read.ctd.itp.md)`()`,
[`read.ctd.odf`](https://dankelley.github.io/oce/reference/read.ctd.odf.md)`()`,
[`read.ctd.odv`](https://dankelley.github.io/oce/reference/read.ctd.odv.md)`()`,
[`read.ctd.saiv`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md)`()`,
[`read.ctd.sbe`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)`()`,
[`read.ctd.ssda`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md)`()`,
[`read.ctd.woce`](https://dankelley.github.io/oce/reference/read.ctd.woce.md)`()`,
[`read.ctd.woce.other`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md)`()`,
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md)`()`,
[`woceUnit2oceUnit`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)`()`,
[`write.ctd`](https://dankelley.github.io/oce/reference/write.ctd.md)`()`

## Examples

``` r
ctd <- read.ctd.aml(system.file("extdata", "ctd_aml.csv.gz", package="oce"))
summary(ctd)
#> CTD Summary
#> -----------
#> 
#> * File:                "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpSsGnPb/temp_libpath8f1f3aac60fb/oce/extdata/ctd_aml.csv.gz"
#> * Mean Location:       70.228N 145.85W
#> * Time: 2021-07-25 18:22:28 to 2021-07-25 18:22:30 (50 samples, mean increment 0.04020408 s)
#> * Data Overview
#> 
#>                              Min.                Mean                Max.                Dim. NAs OriginalName          
#>     scan                     1                   25.5                50                  50   0   "-"                   
#>     salinity [PSS-78]        5.5779              26.323              28.503              50   0   "-"                   
#>     temperature [°C, ITS-90] 3.414               4.8729              5.671               50   0   "Temperature (C)"     
#>     pressure [dbar]          0.22                0.8592              1.69                50   0   "-"                   
#>     conductivity [mS/cm]     6.281               25.687              27.139              50   0   "Conductivity (mS/cm)"
#>     time                     2021-07-25 18:22:28 2021-07-25 18:22:29 2021-07-25 18:22:30 50   0   "-"                   
#>     Date                     NA                  NA                  NA                  50   0   "Date"                
#>     Time                     NA                  NA                  NA                  50   0   "Time"                
#>     battery [V]              7.93                7.93                7.93                50   0   "Battery (V)"         
#> 
#> * Processing Log
#> 
#>     - 2024-09-06 11:48:28 UTC: `create 'ctd' object`
#>     - 2024-09-06 11:48:28 UTC: `as.ctd(salinity = S, temperature = data$temperature, pressure = data$pressure,     conductivity = data$conductivity, serialNumber = serialNumber,     longitude = longitude, latitude = latitude, debug = debug -         1L)`
#>     - 2024-09-06 11:48:28 UTC: `read.ctd.aml(file = system.file("extdata", "ctd_aml.csv.gz",     package = "oce"))`
plot(ctd)

```
