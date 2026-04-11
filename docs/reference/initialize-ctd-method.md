# Initialize Storage for a ctd Object

This function creates
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects.
It is mainly used by `oce` functions such as
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
and [`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
and it is not intended for novice users, so it may change at any time,
without following the usual rules for transitioning to deprecated and
defunct status (see
[oce-deprecated](https://dankelley.github.io/oce/reference/oce-deprecated.md)).

## Usage

``` r
# S4 method for class 'ctd'
initialize(
  .Object,
  pressure,
  salinity,
  temperature,
  conductivity,
  units,
  pressureType,
  deploymentType,
  ...
)
```

## Arguments

- .Object:

  the string `"ctd"`

- pressure:

  optional numerical vector of pressures.

- salinity:

  optional numerical vector of salinities.

- temperature:

  optional numerical vector of temperatures.

- conductivity:

  optional numerical vector of conductivities.

- units:

  optional list indicating units for the quantities specified in the
  previous arguments. If this is not supplied, a default is set up,
  based on which of the `pressure` to `conductivity` arguments were
  specified. If all of those 4 arguments were specified, then `units` is
  set up as if the call included the following:
  `units=list(temperature=list(unit=expression(degree*C), scale="ITS-90"), salinity=list(unit=expression(), scale="PSS-78"), conductivity=list(unit=expression(), scale=""), pressure=list(unit=expression(dbar), scale=""), depth=list(unit=expression(m), scale=""))`.
  This list is trimmed of any of the 4 items that were not specified in
  the previous arguments. Note that if `units` is specified, then it is
  just copied into the `metadata` slot of the returned object, so the
  user must be careful to set up values that will make sense to other
  `oce` functions.

- pressureType:

  optional character string indicating the type of pressure; if not
  supplied, this defaults to `"sea"`, which indicates the excess of
  pressure over the atmospheric value, in dbar.

- deploymentType:

  optional character string indicating the type of deployment, which may
  be `"unknown"`, `"profile"`, `"towyo"`, or `"thermosalinograph"`. If
  this is not set, the value defaults to `"unknown"`.

- ...:

  Ignored.

## Details

To save storage, this function has arguments only for quantities that
are often present in data files all cases. For example, not all data
files will have oxygen, so that's not present here. Extra data may be
added after the object is created, using
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md).
Similarly,
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)
may be used to add metadata (station ID, etc), while bearing in mind
that other functions look for such information in very particular places
(e.g. the station ID is a string named `station` within the `metadata`
slot). See [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
for more information on elements stored in `ctd` objects.

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
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
# 1. empty
new("ctd")
#> ctd object has nothing in its data slot.

# 2. fake data with no location information, so can only
#    plot with the UNESCO equation of state.
#    NOTE: always name arguments, in case the default order gets changed
ctd <- new("ctd", salinity = 35 + 1:3 / 10, temperature = 10 - 1:3 / 10, pressure = 1:3)
summary(ctd)
#> CTD Summary
#> -----------
#> 
#> * Data Overview
#> 
#>                              Min. Mean Max. Dim. NAs
#>     pressure [dbar]          1    2    3    3    0  
#>     temperature [°C, ITS-90] 9.7  9.8  9.9  3    0  
#>     salinity [PSS-78]        35.1 35.2 35.3 3    0  
#> 
#> * Processing Log
#> 
#>     - 2026-04-11 11:45:14 UTC: `create 'ctd' object`
plot(ctd, eos = "unesco")


# 3. as 2, but insert location and plot with GSW equation of state.
ctd <- oceSetMetadata(ctd, "latitude", 44)
ctd <- oceSetMetadata(ctd, "longitude", -63)
plot(ctd, eos = "gsw")

```
