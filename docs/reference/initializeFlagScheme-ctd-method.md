# Establish a Data-Quality Scheme for a ctd Object

This function adds an item named `flagScheme` to the `metadata` slot of
an object inheriting from
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md). This is a
list containing two items: `name` and `mapping`, as provided in the
function arguments. The purpose is both to document a flag scheme and to
make it so that
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md),
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md)
and
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md)
can specify flags by name, as opposed to number. This is a generic
function, that may be specialized to the class of `object` (see
“Details”).

## Usage

``` r
# S4 method for class 'ctd'
initializeFlagScheme(
  object,
  name = NULL,
  mapping = NULL,
  default = NULL,
  update = NULL,
  debug = 0
)
```

## Arguments

- object:

  An oce object.

- name:

  a character value naming the scheme. If this refers to a pre-defined
  scheme, then `mapping` must not be provided, because doing so would
  contradict the pre-defined scheme, defeating its purpose of providing
  concreteness and clarity.

- mapping:

  a list of named items describing the mapping from flag meaning to flag
  numerical value, e.g `list(good=1, bad=2)` might be used for a
  hypothetical class.

- default:

  an integer vector of flag values that are not considered to be good.
  If this is not provided, but if `name` is `"argo"`, `"BODC"`, `"DFO"`,
  `"WHP bottle"`, or `"WHP CTD"`, then a conservative value will be set
  automatically, equal to the list of flag values that designate bad or
  questionable data. For example, for `name="WHP CTD"`, the setting will
  be `c(1,3,4,5,6,7,9)`, leaving only value `2`, which corresponds with
  "acceptable" in the notation used for that flag scheme.

- update:

  a logical value indicating whether the scheme provided is to update an
  existing scheme. The default value, `FALSE`, prevents such an attempt
  to alter an existing flag scheme, if one is already embedded in
  `object`.

- debug:

  an integer set to 0 for quiet action or to 1 for some debugging.

## Value

An object with the `metadata` slot containing `flagScheme`.

## Details

The following pre-defined schemes are available (note that the names are
simplified from the phrases used in defining documentation):

- `name="argo"` defaults `mapping` to OLD (prior to June 10, 2020)

    list(not_assessed=0, passed_all_tests=1, probably_good=2,
         probably_bad=3, bad=4, averaged=7,
         interpolated=8, missing=9)

NEW (after June 10, 2020)

    list(not_assessed=0, passed_all_tests=1, probably_good=2,
         probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7,
         estimated=8, missing=9)

See reference 1 for a deeper explanation of the meanings of these codes.

- `name="BODC"` defaults `mapping` to

    list(no_quality_control=0, good=1, probably_good=2,
         probably_bad=3, bad=4, changed=5,
         below_detection=6, in_excess=7, interpolated=8,
         missing=9)

See reference 2 for a deeper explanation of the meanings of these codes,
and note that codes `A` and `Q` are not provided in oce.

- `name="DFO"` defaults `mapping` to

    list(no_quality_control=0, appears_correct=1, appears_inconsistent=2,
         doubtful=3, erroneous=4, changed=5,
         qc_by_originator=8, missing=9)

See reference 3 for a deeper explanation of the meanings of these codes.

- `name="WHP bottle"` defaults `mapping` to

    list(no_information=1, no_problems_noted=2, leaking=3,
         did_not_trip=4, not_reported=5, discrepency=6,
         unknown_problem=7, did_not_trip=8, no_sample=9)

See reference 4 for a deeper explanation of the meanings of these codes.

- `name="WHP CTD"` defaults `mapping` to

    list(not_calibrated=1, acceptable=2, questionable=3,
        bad=4, not_reported=5, interpolated=6,
        despiked=7, missing=9)

See reference 4 for a deeper explanation of the meanings of these codes.

## References

1.  The codes for `"argo"` are derived from information in Table 4.1 of
    Wong, Annie, Robert Keeley, Thierry Carval, and Argo Data Management
    Team (8 January 2020), "Argo Quality Control Manual for CTD and
    Trajectory Data, Version 3.3," available at
    `https://archimer.ifremer.fr/doc/00228/33951/` as of June 2020.

2.  The codes for `"BODC"` are defined at
    http://seadatanet.maris2.nl/v_bodc_vocab_v2/browse.asp?order=conceptid&formname=search&screen=0&lib=l20

3.  The codes for `"DFO"` are defined at
    http://www.dfo-mpo.gc.ca/science/data-donnees/code/list/014-eng.html

4.  The codes for `"WHP CTD"` and `"WHP bottle"` are defined at
    https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm

## See also

Other functions relating to data-quality flags:
[`defaultFlags()`](https://dankelley.github.io/oce/reference/defaultFlags.md),
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`handleFlags,oce-method`](https://dankelley.github.io/oce/reference/handleFlags-oce-method.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme()`](https://dankelley.github.io/oce/reference/initializeFlagScheme.md),
[`initializeFlagScheme,oce-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-oce-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`initializeFlagSchemeInternal()`](https://dankelley.github.io/oce/reference/initializeFlagSchemeInternal.md),
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md),
[`initializeFlags,adp-method`](https://dankelley.github.io/oce/reference/initializeFlags-adp-method.md),
[`initializeFlags,oce-method`](https://dankelley.github.io/oce/reference/initializeFlags-oce-method.md),
[`initializeFlagsInternal()`](https://dankelley.github.io/oce/reference/initializeFlagsInternal.md),
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`setFlags,oce-method`](https://dankelley.github.io/oce/reference/setFlags-oce-method.md)

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
