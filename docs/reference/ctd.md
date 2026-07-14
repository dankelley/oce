# Sample ctd Data

This is a CTD profile measured in Halifax Harbour in 2003, based on
[`ctdRaw()`](https://dankelley.github.io/oce/reference/ctdRaw.md), but
trimmed to just the downcast with
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md),
using indices inferred by inspection of the results from
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md).

## Usage

``` r
data(ctd)
```

## Details

This station was sampled by students enrolled in the Dan Kelley's
Physical Oceanography class at Dalhousie University. The data were
acquired near the centre of the Bedford Basin of the Halifax Harbour,
during an October 2003 field trip of Dalhousie University's Oceanography
4120/5120 class. Note that the `startTime` in the `metadata` slot was
altered from 1903 to 2003, using
[`oceEdit()`](https://dankelley.github.io/oce/reference/oceEdit.md). The
change was done because the original time was clearly incorrect, perhaps
owing to the use of software that was designed to work in the twentieth
century only.

## Sample of Usage


    library(oce)
    data(ctd)
    plot(ctd)

## See also

The full profile (not trimmed to the downcast) is available as
`data(ctdRaw)`.

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
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

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
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
