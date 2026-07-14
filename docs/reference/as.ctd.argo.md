# Coerce argo Data Into a ctd Object

Assemble argo data into a
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.
This function may be called by
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) or
called directly. In the first case, note that the only
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md)
parameters that are provide are the object, the profile number, and the
debug value.

## Usage

``` r
as.ctd.argo(argo, profile = NULL, debug = getOption("oceDebug"))
```

## Arguments

- argo:

  an [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  object.

- profile:

  an integer specifying the profile to pick within the argo object. This
  will be set to 1, with a warning, if it is not supplied.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## See also

Other things related to argo data:
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argoGrid`](https://dankelley.github.io/oce/reference/argoGrid.md)`()`,
[`argoNames2oceNames`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md)`()`,
[`as.argo`](https://dankelley.github.io/oce/reference/as.argo.md)`()`,
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo`](https://dankelley.github.io/oce/reference/read.argo.md)`()`,
[`read.argo.copernicus`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md)`()`,
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

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
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
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

## Author

Dan Kelley
