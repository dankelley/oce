# Extract Something From a ctd Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'ctd'
x[[i, j, ...]]
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
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

Some uses of `[[,ctd-method` involve direct retrieval of items within
the `data` slot of the `ctd` object, while other uses involve
calculations based on items in that `data` slot. For example, all `ctd`
objects should hold an item named `temperature` in the `data` slot, so
for example `x[["temperature"]]` will retrieve that item. By contrast,
`x[["sigmaTheta"]]` is taken to be a request to compute
\\\sigma\_\theta\\, and so it yields a call to
[swTheta](https://dankelley.github.io/oce/reference/swTheta.md)`(x)`
even if the `data` slot of `x` might happen to contain an item named
`theta`. This can be confusing at first, but it tends to lead to fewer
surprises in everyday work, for otherwise the user would be forced to
check the contents of any `ctd` object under analysis, to determine
whether that item will be looked up or computed. Nothing is lost in this
scheme, since the data within the object are always accessible with
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md).

It should be noted that the accessor is set up to retrieve quantities in
conventional units. For example,
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
is used on a `.cnv` file that stores pressure in psi, it will be stored
in the same unit within the `ctd` object, but `x[["pressure"]]` will
return a value that has been converted to decibars. (To get pressure in
PSI, use `x[["pressurePSI"]]`.) Similarly, temperature is returned in
the ITS-90 scale, with a conversion having been performed with
[`T90fromT68()`](https://dankelley.github.io/oce/reference/T90fromT68.md),
if the object holds temperature in IPTS-68. Again, temperature on the
IPTS-68 scale is returned with `x@data$temperature`.

This preference for computed over stored quantities is accomplished by
first checking for computed quantities, and then falling back to the
general `[[` method if no match is found.

Some quantities are optionally computed. For example, some data files
(e.g. the one upon which the
[`section()`](https://dankelley.github.io/oce/reference/section.md)
dataset is based) store `nitrite` along with the sum of nitrite and
nitrate, the latter with name `NO2+NO3`. In this case, e.g.
`x[["nitrate"]]` will detect the setup, and subtract nitrite from the
sum to yield nitrate.

The list given below provides notes on some quantities that are
available using e.g. `ctd[[i]]`.

- If `i` is `"?"`, then the return value is a list containing four
  items, each of which is a character vector holding the names of things
  that can be accessed with `[[`. The `data` and `metadata` items hold
  the names of entries in the object's data and metadata slots,
  respectively. The `dataDerived` and `metadataDerived` items hold the
  names of things that can be inferred from the object's contents, e.g.
  `"SA"` is named in `dataDerived`, indicating that `argo[["SA"]]` is
  permitted (to compute Absolute Salinity).

- If `i` is `"conductivity"` without a second argument (e.g.
  `a[["conductivity"]]`) then the return value is the seawater
  electrical conductivity (if available or computable). However, if a
  second argument is given, and it is string specifying a unit, then
  conversion is made to that unit. The permitted units are: either `""`
  or `"ratio"` (for ratio), `"uS/cm"`, `"mS/cm"` and `"S/m"`. The
  calculations are based on the definition of conductivity ratio as the
  ratio between measured conductivity and the standard value 4.2914 S/m.

- If `i` is `"CT"` or `"Conservative Temperature"` then Conservative
  Temperature, computed with
  [`gsw::gsw_CT_from_t()`](http://teos-10.github.io/GSW-R/reference/gsw_CT_from_t.md),
  is returned.

- If `i` is `"density"` then seawater density, computed with
  [swRho](https://dankelley.github.io/oce/reference/swRho.md)`(x)`, is
  returned. (Note that it may be better to call that function directly,
  to gain control of the choice of equation of state, etc.)

- If `i` is `"depth"` then the depth in metres below the surface,
  computed with
  [swDepth](https://dankelley.github.io/oce/reference/swDepth.md)`(x)`,
  is returned.

- If `i` is `"N2"` then the square of Brunt-Vaisala frequency, computed
  with [swN2](https://dankelley.github.io/oce/reference/swN2.md)`(x)`,
  is returned.

- If `i` is `"potential temperature"` or `"theta"`, then potential
  temperature in the UNESCO formulation, computed with
  [swTheta](https://dankelley.github.io/oce/reference/swTheta.md)`(x)`,
  is returned.

- If `i` is `"Rrho"` then density ratio, computed with
  [swRrho](https://dankelley.github.io/oce/reference/swRrho.md)`(x)`, is
  returned.

- If `i` is `"SA"` or `"Absolute Salinity"` then Absolute Salinity,
  computed with
  [`gsw::gsw_SA_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_SA_from_SP.md),
  is returned. The calculation involves location as well as measured
  water properties. If the object `x` does not containing information on
  the location, then 30N and 60W is used for the calculation, and a
  warning is generated.

- If `i` is `"sigmaTheta"` then a form of potential density anomaly,
  computed with
  [swSigmaTheta](https://dankelley.github.io/oce/reference/swSigmaTheta.md)`(x)`,
  is returned.

- If `i` is `"sigma0"` then potential density anomaly referenced to a
  sea pressure of 0dbar (the surface), computed with
  [swSigma0](https://dankelley.github.io/oce/reference/swSigma0.md)`(x)`,
  is returned.

- If `i` is `"sigma2"` then potential density anomaly referenced to a
  sea pressure of 1000dbar, computed with
  [swSigma1](https://dankelley.github.io/oce/reference/swSigma1.md)`(x)`,
  is returned.

- If `i` is `"sigma2"` then potential density anomaly referenced to a
  sea pressure of 2000dbar, computed with
  [swSigma2](https://dankelley.github.io/oce/reference/swSigma2.md)`(x)`,
  is returned.

- If `i` is `"sigma3"` then potential density anomaly referenced to a
  sea pressure of 3000dbar, computed with
  [swSigma3](https://dankelley.github.io/oce/reference/swSigma3.md)`(x)`,
  is returned.

- If `i` is `"sigma4"` then potential density anomaly referenced to a
  sea pressure of 4000dbar, computed with
  [swSigma4](https://dankelley.github.io/oce/reference/swSigma4.md)`(x)`,
  is returned.

- If `i` is `"SP"` then salinity on the Practical Salinity Scale, which
  is `salinity` in the `data` slot, is returned.

- If `i` is `"spice"` then
  [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md) is
  called to compute a quantity that is in some sense orthogonal to
  density on a T-S diagram. This is done by calling
  [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
  with the `eos` argument set to `"unesco"`. In an earlier version of
  oce, `[[` could be provided with a second argument to yield a return
  value for "spiciness", a variable that is described in the next item.
  On 2024-02-14, this possibility was removed because it could lead to
  user confusion and non-reproducible code. To get spiciness, use
  `[["spiciness0"]]`.

- If `i` is `"spiciness0"`, `"spiciness1"` or `"spiciness2"`, then the
  return value comes from the Gibbs SeaWater formulation of a variable
  that is in some sense orthogonal to density on a T-S diagram. The
  numbers refer to the reference pressure, in units of 1000 dbar. These
  results are computed with
  [`gsw::gsw_spiciness0()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness0.md),
  etc.

- If `i` is `"SR"` then Reference Salinity, computed with
  [`gsw::gsw_SR_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_SR_from_SP.md),
  is returned.

- If `i` is `"Sstar"` then Preformed Salinity, computed with
  [`gsw::gsw_SR_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_SR_from_SP.md),
  is returned. See `SA` for a note on longitude and latitude.

- If `i` is `"time"` then either vector of times or a single time, is
  returned, if available. A vector is returned if `time` is present in
  the `data` slot, or if a time can be inferred from other entries in
  the `data` slot (some of which, such as the common `timeS`, also
  employ `startTime` within the `metadata` slot). A single value is
  returned if the dataset only has information on the start time (which
  is stored as `startTime` within the `metadata` slot. If it is
  impossible to determine the sampling time, then `NULL` is returned.
  These time variants occur, in the present version of oce, only for
  data read by
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
  the documentation of which explains how times are computed.

- If `i` is `"z"` then vertical coordinate in metres above the surface,
  computed with
  [swZ](https://dankelley.github.io/oce/reference/swZ.md)`(x)`, is
  returned.

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
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
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

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
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
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

## Author

Dan Kelley

## Examples

``` r
data(ctd)
head(ctd[["temperature"]])
#> [1] 14.22109 14.22649 14.22509 14.22219 14.22669 14.23318
```
