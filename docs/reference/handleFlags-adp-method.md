# Handle Flags in adp Objects

Data-quality flags are stored in the `metadata` slot of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects in
a [list](https://rdrr.io/r/base/list.html) named `flags`. The present
function (a generic that has specialized versions for various data
classes) provides a way to manipulate the contents of the `data` slot,
based on such data-quality flags. For example, a common operation is to
replace erroneous data with `NA`.

If the `flags` within `object`'s `metadata` slot is empty, then `object`
is returned, unaltered. Otherwise, `handleFlags` examines
`object@metadata$flags` in the context of the `flags` argument, and then
carries out actions that are specified by the `actions` argument. By
default, this sets the returned `data` entries to `NA`, wherever the
corresponding `metadata$flag` values signal unreliable data. To maintain
a hint as to why `data` were changed, `metadata$flags` in the returned
value is a direct copy of the corresponding entry in `object`.

## Usage

``` r
# S4 method for class 'adp'
handleFlags(
  object = "oce",
  flags = NULL,
  actions = NULL,
  where = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- flags:

  A [list](https://rdrr.io/r/base/list.html) specifying flag values upon
  which actions will be taken. This can take two forms.

  - In the first form, the list has named elements each containing a
    vector of integers. For example, salinities flagged with values of 1
    or 3:9 would be specified by `flags=list(salinity=c(1,3:9))`.
    Several data items can be specified, e.g.
    `flags=list(salinity=c(1,3:9), temperature=c(1,3:9))` indicates that
    the actions are to take place for both salinity and temperature.

  - In the second form, `flags` is a list holding a single *unnamed*
    vector, and this means to apply the actions to *all* the data
    entries. For example, `flags=list(c(1,3:9))` means to apply not just
    to salinity and temperature, but to everything within the `data`
    slot.

  If `flags` is not provided, then
  [`defaultFlags()`](https://dankelley.github.io/oce/reference/defaultFlags.md)
  is called, to try to determine a reasonable default.

- actions:

  an optional [list](https://rdrr.io/r/base/list.html) that contains
  items with names that match those in the `flags` argument. If
  `actions` is not supplied, the default will be to set all values
  identified by `flags` to `NA`; this can also be specified by
  specifying `actions=list("NA")`. It is also possible to specify
  functions that calculate replacement values. These are provided with
  `object` as the single argument, and must return a replacement for the
  data item in question. See “Details” for the default that is used if
  `actions` is not supplied.

- where:

  an optional character value that permits the function to work with
  objects that store flags in e.g. `object@metadata$flags$where` instead
  of in `object@metadata$flags`, and data within `object@data$where`
  instead of within `object@data`. The default value of `NULL` means to
  look withing `object@metadata` itself, and this is the default within
  `oce`. (The purpose of `where` is to make `oce` extensible by other
  packages, which may choose to store data two levels deep in the `data`
  slot.)

- debug:

  An optional integer specifying the degree of debugging, with value 0
  meaning to skip debugging and 1 or higher meaning to print some
  information about the arguments and the data. It is usually a good
  idea to set this to 1 for initial work with a dataset, to see which
  flags are being handled for each data item. If not supplied, this
  defaults to the value of
  [`getOption`](https://rdrr.io/r/base/options.html)`("oceDebug")`.

## Details

If `flags` and `actions` are not provided, the default is to consider a
flag value of 1 to indicate bad data, and 0 to indicate good data. Note
that it only makes sense to use velocity (`v`) flags, because other
flags are, at least for some instruments, stored as `raw` quantities,
and such quantities may not be set to `NA`.

## See also

Other functions relating to data-quality flags:
[`defaultFlags()`](https://dankelley.github.io/oce/reference/defaultFlags.md),
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md),
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
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
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
# Flag low "goodness" or high "error beam" values.
library(oce)
data(adp)
# Same as Example 2 of ?'setFlags,adp-method'
v <- adp[["v"]]
i2 <- array(FALSE, dim = dim(v))
g <- adp[["g", "numeric"]]
# Set thresholds on percent "goodness" and error "velocity".
G <- 25
V4 <- 0.45
for (k in 1:3) {
    i2[, , k] <- ((g[, , k] + g[, , 4]) < G) | (v[, , 4] > V4)
}
adpQC <- initializeFlags(adp, "v", 2)
adpQC <- setFlags(adpQC, "v", i2, 3)
adpClean <- handleFlags(adpQC, flags = list(3), actions = list("NA"))
# Demonstrate (subtle) change graphically.
par(mfcol = c(2, 1))
plot(adp, which = "u1", drawTimeRange = FALSE)
plot(adpClean, which = "u1", drawTimeRange = FALSE)
t0 <- 1214510000 # from locator()
arrows(t0, 20, t0, 35, length = 0.1, lwd = 3, col = "magenta")
mtext("Slight change above arrow", col = "magenta", font = 2)

```
