# Extract Something From an adp Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'adp'
x[[i, j, ...]]
```

## Arguments

- x:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- i:

  character value indicating the name of an item to extract.

- j:

  optional additional information on the `i` item.

- ...:

  ignored.

## Details

A two-step process is used to try to find the requested information.
First, a class-specific function is used (see “Details of the
Specialized Method”). If this yields nothing, then a general method is
used (see “Details of the General Method”). If both methods fail, then
`[[` returns NULL.

Some understanding of the subclass is required to know what can be
retrieved with `[[`. When dealing with an unfamiliar subclass, it can be
useful to first use `x[["?"]]` to get a listing of the retrievable
items. See “Details of the Specialized Method” for more information.

## Details of the Specialized Method

Note that the entries within
[adp](https://dankelley.github.io/oce/reference/adp-class.md) objects
vary greatly, from instrument to instrument, and so are only sketched
here, and in the output from `[["?"]]`.

- If `i` is `"?"`, then the return value is a list containing four
  items, each of which is a character vector holding the names of things
  that can be accessed with `[[`. The `data` and `metadata` items hold
  the names of entries in the object's data and metadata slots,
  respectively. The `dataDerived` and `metadataDerived` items are *not*
  authoritative, because information provided by different instruments
  is so varied.

- If `i` is `"u1"` then the return value is `v[,1]`. The same holds for
  2, etc., depending on the number of beams in the instrument.

- If `i` is `"a1"` then signal amplitude is returned, and similarly for
  other digits. The results can be in
  [`raw()`](https://rdrr.io/r/base/raw.html) or numeric form, as shown
  in the examples.

- If `i` is `"q1"` then signal quality is returned, and similarly for
  other digits. As with amplitude, the result can be in
  [`raw()`](https://rdrr.io/r/base/raw.html) or numeric form.

- If `i` is `"coordinate"`, then the coordinate system is retrieved.

## Details of the General Method

Note: the text of this section is identical for all `oce` subclasses,
and so some of what you read here may not be relevant to the class being
described in this help page.

If the specialized method produces no matches, the following generalized
method is applied. As with the specialized method, the procedure hinges
first on the values of `i` and, optionally, `j`. The work proceeds in
steps, by testing a sequence of possible conditions in sequence.

1.  A check is made as to whether `i` names one of the standard `oce`
    slots. If so, `[[` returns the slot contents of that slot. Thus,
    `x[["metadata"]]` will retrieve the `metadata` slot, while
    `x[["data"]]` and `x[["processingLog"]]` return those slots.

2.  If `i` is a string ending in the `"Unit"`, then the characters
    preceding that string are taken to be the name of an item in the
    data object, and a list containing the unit is returned (or `NULL`
    if there is no such unit). This list consists of an item named
    `unit`, which is an
    [`expression()`](https://rdrr.io/r/base/expression.html), and an
    item named `scale`, which is a string describing the measurement
    scale. If the string ends in `" unit"`, e.g.
    `x[["temperature unit"]]` (note the space), then just the expression
    is returned, and if it ends in `" scale"`, then just the scale is
    returned.

3.  If `i` is a string ending in `"Flag"`, then the corresponding
    data-quality flag is returned (or `NULL` if there is no such flag).

4.  If the object holds hydrographic information (pressure, salinity,
    temperature, longitude and latitude) then another set of
    possibilities arises. If `i` is `"sigmaTheta"`, then the value of
    [`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md)
    is called with `x` as the sole argument, and the results are
    returned. Similarly,
    [`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md)
    is used if `i="sigma0"`, and
    [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
    is used if `i="spice"`. Of course, these actions only make sense for
    objects that contain the relevant items within their `data` slot.

5.  After these possibilities are eliminated, the action depends on
    whether `j` has been provided. If `j` is not provided, or is the
    string `""`, then `i` is sought in the `metadata` slot, and then in
    the `data` slot, returning whichever is found first. In other words,
    if `j` is not provided, the `metadata` slot takes preference over
    the `data` slot. However, if `j` is provided, then it must be either
    the string `"metadata"` or `"data"`, and it directs where to look.

6.  If none of the above-listed conditions holds, then `NULL` is
    returned.

## See also

Other functions that extract parts of oce objects:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md),
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
[`[[,ladp-method`](https://dankelley.github.io/oce/reference/sub-sub-ladp-method.md),
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
[`[[,oce-method`](https://dankelley.github.io/oce/reference/sub-sub-oce-method.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
[`[[,windrose-method`](https://dankelley.github.io/oce/reference/sub-sub-windrose-method.md),
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,adv-method`

Other things related to adp data: `[[<-,adp-method`,
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

## Author

Dan Kelley

## Examples

``` r
data(adp)
# Tests for beam 1, distance bin 1, first 5 observation times
adp[["v"]][1:5, 1, 1]
#> [1] -0.11955771 -0.09925398  0.10203802  0.09613003  0.24394126
adp[["a"]][1:5, 1, 1]
#> [1] 99 9d 9f 87 8a
adp[["a", "numeric"]][1:5, 1, 1]
#> [1] 153 157 159 135 138
as.numeric(adp[["a"]][1:5, 1, 1]) # same as above
#> [1] 153 157 159 135 138
```
