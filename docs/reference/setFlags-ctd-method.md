# Set Data-Quality Flags within a ctd Object

This function changes specified entries in the data-quality flags of a
ctd object, which are stored within a list named `flags` that resides in
the `metadata` slot. If the object already has a flag set up for `name`,
then only the specified entries are altered. If not, the flag entry is
first created and its entries set to `default`, after which the entries
specified by `i` are changed to `value`.

The specification is made with `i`, the form of which is determined by
the data item in question. Generally, the rules are as follows:

1.  If the data item is a vector, then `i` must be (a) an integer vector
    specifying indices to be set to `value`, (b) a logical vector of
    length matching the data item, with `TRUE` meaning to set the flag
    to `value`, or (c) a function that takes an `oce` object as its
    single argument, and returns a vector in either of the forms just
    described.

2.  If the data item is an array, then `i` must be (a) a data frame of
    integers whose rows specify spots to change (where the number of
    columns matches the number of dimensions of the data item), (b) a
    logical array that has dimension equal to that of the data item,
    or (c) a function that takes an `oce` object as its single input and
    returns such a data frame or array.

See “Details” for the particular case of
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects.

## Usage

``` r
# S4 method for class 'ctd'
setFlags(
  object,
  name = NULL,
  i = NULL,
  value = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  An oce object.

- name:

  Character string indicating the name of the variable to be flagged. If
  this variable is not contained in the object's `data` slot, an error
  is reported.

- i:

  Indication of where to insert the flags; see “Description” for general
  rules and “Details” for rules for
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects.

- value:

  The value to be inserted in the flag.

- debug:

  Integer set to 0 for quiet action or to 1 for some debugging.

## Value

An object with flags set as indicated.

## Details

Since all the entries in the `data` slot of ctd objects are vectors, `i`
must be a vector (either logical as in Example 1 or integer as in
Example 2), or a function taking a `ctd` object and returning such a
vector (see “Indexing rules”).

## Sample of Usage

    # Example 2: Interactive flag assignment based on TS plot, using
    # WHP scheme to define 'acceptable' and 'bad' codes
    options(eos="gsw")
    data(ctd)
    qc <- ctd
    qc <- initializeFlagScheme(qc, "WHP CTD")
    qc <- initializeFlags(qc, "salinity", 2)
    Sspan <- diff(range(qc[["SA"]]))
    Tspan <- diff(range(qc[["CT"]]))
    n <- length(qc[["SA"]])
    par(mfrow=c(1, 1))
    plotTS(qc, type="o")
    message("Click on bad points; quit by clicking to right of plot")
    for (i in seq_len(n)) {
        xy <- locator(1)
        if (xy$x > par("usr")[2])
            break
        i <- which.min(abs(qc[["SA"]] - xy$x)/Sspan + abs(qc[["CT"]] - xy$y)/Tspan)
        qc <- setFlags(qc, "salinity", i=i, value=4)
        qc <- handleFlags(qc, flags=list(salinity=4))
        plotTS(qc, type="o")
    }

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
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`initializeFlagScheme,oce-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-oce-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`initializeFlagSchemeInternal()`](https://dankelley.github.io/oce/reference/initializeFlagSchemeInternal.md),
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md),
[`initializeFlags,adp-method`](https://dankelley.github.io/oce/reference/initializeFlags-adp-method.md),
[`initializeFlags,oce-method`](https://dankelley.github.io/oce/reference/initializeFlags-oce-method.md),
[`initializeFlagsInternal()`](https://dankelley.github.io/oce/reference/initializeFlagsInternal.md),
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
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
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Example 1: Range-check salinity
data(ctdRaw)
# Salinity and temperature range checks
qc <- ctdRaw
# Initialize flags to 2, meaning good data in the default
# scheme for handleFlags(ctd).
qc <- initializeFlags(qc, "salinity", 2)
qc <- initializeFlags(qc, "temperature", 2)
# Flag bad salinities as 4
oddS <- with(qc[["data"]], salinity < 25 | 40 < salinity)
qc <- setFlags(qc, name = "salinity", i = oddS, value = 4)
# Flag bad temperatures as 4
oddT <- with(qc[["data"]], temperature < -2 | 40 < temperature)
qc <- setFlags(qc, name = "temperature", i = oddT, value = 4)
# Compare results in TS space
par(mfrow = c(2, 1))
plotTS(ctdRaw)
plotTS(handleFlags(qc, flags = c(1, 3:9)))

```
