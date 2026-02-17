# Replace Parts of a ctd Object

The `[[<-` method works for all
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.
The purpose, as with the related extraction method, `[[`, is to insulate
users from the internal details of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects,
by looking for items within the various storage slots of the object.
Items not actually stored can also be replaced, including units and
data-quality flags.

## Usage

``` r
# S4 method for class 'ctd'
x[[i, j, ...]] <- value
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
  object.

- i:

  character value naming the item to replace.

- j:

  optional additional information on the `i` item.

- ...:

  optional additional information (ignored).

- value:

  The value to be placed into `x`, somewhere.

## Details

As with `[[` method, the procedure works in steps.

First, the `metadata` slot of `x` is checked to see whether it contains
something named with `i`. If so, then the named item is replaced with
`value`.

Otherwise, if the string value of `i` ends in `Unit`, then the
characters preceding that are taken as the name of a variable, and the
`metadata` slot of `x` is updated to store that unit, e.g.

    x[["temperatureUnits"]] <- list(unit=expression(degree*F),scale="")

Similarly, if `i` ends in `Flag`, then quality-control flags are set up
as defined by `result`, e.g.

    o[["temperatureFlags"]] <- c(2,4,2,2)

Otherwise, [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used for
a partial-string match with the names of the items that are in the
`data` slot of `x`. The first item found (if any) is then updated to
hold the value `result`.

If none of these conditions is met, a warning is issued.

## See also

Other functions that replace parts of oce objects: `[[<-,adp-method`,
`[[<-,amsr-method`, `[[<-,argo-method`, `[[<-,bremen-method`,
`[[<-,cm-method`, `[[<-,coastline-method`, `[[<-,echosounder-method`,
`[[<-,g1sst-method`, `[[<-,gps-method`, `[[<-,ladp-method`,
`[[<-,landsat-method`, `[[<-,lisst-method`, `[[<-,lobo-method`,
`[[<-,met-method`, `[[<-,oce-method`, `[[<-,odf-method`,
`[[<-,rsk-method`, `[[<-,sealevel-method`, `[[<-,section-method`,
`[[<-,tidem-method`, `[[<-,topo-method`, `[[<-,windrose-method`,
`[[<-,xbt-method`

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
[`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`ctdRepair()`](https://dankelley.github.io/oce/reference/ctdRepair.md),
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md),
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md),
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.odv()`](https://dankelley.github.io/oce/reference/read.ctd.odv.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

## Examples

``` r
data(ctd)
summary(ctd)
#> CTD Summary
#> -----------
#> 
#> * File:                "/Users/kelley/git/oce/create_data/ctd/ctd.cnv"
#> * Instrument:          SBE 25
#> * Temp. serial no.:    1140
#> * Cond. serial no.:    832
#> * Original file:       c:\seasoft3\basin\bed0302.hex
#> * Start time:          2003-10-15 15:38:38
#> * Sample interval:     1 s
#> * Cruise:              Halifax Harbour
#> * Vessel:              Divcom3
#> * Station:             Stn 2
#> * Mean Location:       44.684N 63.644W
#> * Data Overview
#> 
#>                               Min.   Mean   Max.   Dim. NAs OriginalName
#>     scan                      130    220    310    181  0   "scan"      
#>     timeS [s]                 129    219    309    181  0   "timeS"     
#>     pressure [dbar]           1.48   22.885 44.141 181  0   "pr"        
#>     depth [m]                 1.468  22.698 43.778 181  0   "depS"      
#>     temperature [°C, IPTS-68] 2.919  7.5063 14.237 181  0   "t068"      
#>     salinity [PSS-78]         29.916 31.219 31.498 181  0   "sal00"     
#>     flag                      0      0      0      181  0   "flag"      
#> 
#> * Processing Log
#> 
#>     - 2018-11-14 20:03:47 UTC: `create 'ctd' object`
#>     - 2018-11-14 20:03:47 UTC: `read.ctd.sbe(file = file, debug = 10, processingLog = processingLog)`
#>     - 2018-11-14 20:03:47 UTC: `oce.edit(x = ctd, item = "startTime", value = as.POSIXct(gsub("1903",     "2003", format(ctd[["startTime"]])), tz = "UTC") + 4 * 3600,     reason = "file had year=1903, instead of 2003", person = "Dan Kelley")`
# Move the CTD profile a nautical mile north.
ctd[["latitude"]] <- 1 / 60 + ctd[["latitude"]] # acts in metadata
# Increase the salinity by 0.01.
ctd[["salinity"]] <- 0.01 + ctd[["salinity"]] # acts in data
summary(ctd)
#> CTD Summary
#> -----------
#> 
#> * File:                "/Users/kelley/git/oce/create_data/ctd/ctd.cnv"
#> * Instrument:          SBE 25
#> * Temp. serial no.:    1140
#> * Cond. serial no.:    832
#> * Original file:       c:\seasoft3\basin\bed0302.hex
#> * Start time:          2003-10-15 15:38:38
#> * Sample interval:     1 s
#> * Cruise:              Halifax Harbour
#> * Vessel:              Divcom3
#> * Station:             Stn 2
#> * Mean Location:       44.701N 63.644W
#> * Data Overview
#> 
#>                               Min.   Mean   Max.   Dim. NAs OriginalName
#>     scan                      130    220    310    181  0   "scan"      
#>     timeS [s]                 129    219    309    181  0   "timeS"     
#>     pressure [dbar]           1.48   22.885 44.141 181  0   "pr"        
#>     depth [m]                 1.468  22.698 43.778 181  0   "depS"      
#>     temperature [°C, IPTS-68] 2.919  7.5063 14.237 181  0   "t068"      
#>     salinity [PSS-78]         29.926 31.229 31.508 181  0   "sal00"     
#>     flag                      0      0      0      181  0   "flag"      
#> 
#> * Processing Log
#> 
#>     - 2018-11-14 20:03:47 UTC: `create 'ctd' object`
#>     - 2018-11-14 20:03:47 UTC: `read.ctd.sbe(file = file, debug = 10, processingLog = processingLog)`
#>     - 2018-11-14 20:03:47 UTC: `oce.edit(x = ctd, item = "startTime", value = as.POSIXct(gsub("1903",     "2003", format(ctd[["startTime"]])), tz = "UTC") + 4 * 3600,     reason = "file had year=1903, instead of 2003", person = "Dan Kelley")`
```
