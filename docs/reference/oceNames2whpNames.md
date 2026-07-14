# Translate Oce Data Names to WHP Data Names

Translate oce-style names to WOCE names, using
[`gsub()`](https://rdrr.io/r/base/grep.html) to match patterns. For
example, the pattern `"oxygen"` is taken to mean `"CTDOXY"`.

## Usage

``` r
oceNames2whpNames(names)
```

## Arguments

- names:

  vector of strings holding oce-style names.

## Value

vector of strings holding WHP-style names.

## References

Several online sources list WHP names. An example is
`https://cchdo.github.io/hdo-assets/documentation/manuals/pdf/90_1/chap4.pdf`

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
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
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

Other functions that interpret variable names and units from headers:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`unitFromString()`](https://dankelley.github.io/oce/reference/unitFromString.md),
[`unitFromStringRsk()`](https://dankelley.github.io/oce/reference/unitFromStringRsk.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)

## Author

Dan Kelley
