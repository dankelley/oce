# Set Data-Quality Flags within a adp Object

This function changes specified entries in the data-quality flags of a
adp object, which are stored within a list named `flags` that resides in
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
[adp](https://dankelley.github.io/oce/reference/adp-class.md) objects.

## Usage

``` r
# S4 method for class 'adp'
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
  [adp](https://dankelley.github.io/oce/reference/adp-class.md) objects.

- value:

  The value to be inserted in the flag.

- debug:

  Integer set to 0 for quiet action or to 1 for some debugging.

## Value

An object with flags set as indicated.

## Details

The only flag that may be set is `v`, for the array holding velocity.
See “Indexing rules”, noting that adp data are stored in 3D arrays;
Example 1 shows using a data frame for `i`, while Example 2 shows using
an array.

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
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`setFlags,oce-method`](https://dankelley.github.io/oce/reference/setFlags-oce-method.md)

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpConvertRawToNumeric()`](https://dankelley.github.io/oce/reference/adpConvertRawToNumeric.md),
[`adpEnsembleAverage()`](https://dankelley.github.io/oce/reference/adpEnsembleAverage.md),
[`adpFlagPastBoundary()`](https://dankelley.github.io/oce/reference/adpFlagPastBoundary.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`as.adp()`](https://dankelley.github.io/oce/reference/as.adp.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`beamToXyzAdp()`](https://dankelley.github.io/oce/reference/beamToXyzAdp.md),
[`beamToXyzAdpAD2CP()`](https://dankelley.github.io/oce/reference/beamToXyzAdpAD2CP.md),
[`beamToXyzAdv()`](https://dankelley.github.io/oce/reference/beamToXyzAdv.md),
[`beamUnspreadAdp()`](https://dankelley.github.io/oce/reference/beamUnspreadAdp.md),
[`binmapAdp()`](https://dankelley.github.io/oce/reference/binmapAdp.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdp()`](https://dankelley.github.io/oce/reference/enuToOtherAdp.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md),
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subtractBottomVelocity()`](https://dankelley.github.io/oce/reference/subtractBottomVelocity.md),
[`summary,adp-method`](https://dankelley.github.io/oce/reference/summary-adp-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdp()`](https://dankelley.github.io/oce/reference/toEnuAdp.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdp()`](https://dankelley.github.io/oce/reference/xyzToEnuAdp.md),
[`xyzToEnuAdpAD2CP()`](https://dankelley.github.io/oce/reference/xyzToEnuAdpAD2CP.md)

## Examples

``` r
library(oce)
data(adp)

# Example 1: flag first 10 samples in a mid-depth bin of beam 1
i1 <- data.frame(1:20, 40, 1)
adpQC <- initializeFlags(adp, "v", 2)
adpQC <- setFlags(adpQC, "v", i1, 3)
adpClean1 <- handleFlags(adpQC, flags = list(3), actions = list("NA"))
par(mfrow = c(2, 1))
# Top: original, bottom: altered
plot(adp, which = "u1")
plot(adpClean1, which = "u1")


# Example 2: percent-good and error-beam scheme
v <- adp[["v"]]
i2 <- array(FALSE, dim = dim(v))
g <- adp[["g", "numeric"]]
# Thresholds on percent "goodness" and error "velocity"
G <- 25
V4 <- 0.45
for (k in 1:3) {
    i2[, , k] <- ((g[, , k] + g[, , 4]) < G) | (v[, , 4] > V4)
}
adpQC2 <- initializeFlags(adp, "v", 2)
adpQC2 <- setFlags(adpQC2, "v", i2, 3)
adpClean2 <- handleFlags(adpQC2, flags = list(3), actions = list("NA"))
# Top: original, bottom: altered
plot(adp, which = "u1")
plot(adpClean2, which = "u1") # differs at 8h and 20h

```
