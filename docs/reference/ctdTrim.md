# Trim Beginning and Ending of a CTD cast

Often in CTD profiling, the goal is to isolate only the downcast,
discarding measurements made in the air, in an equilibration phase in
which the device is held below the water surface, and then the upcast
phase that follows the downcast. This is handled reasonably well by
`ctdTrim` with `method="downcast"`, although it is almost always best to
use
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md) to
investigate the data, and then use the `method="index"` or
`method="scan"` method based on visual inspection of the data.

## Usage

``` r
ctdTrim(
  x,
  method,
  removeDepthInversions = FALSE,
  parameters = NULL,
  indices = FALSE,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
  object.

- method:

  A string (or a vector of two strings) specifying the trimming method,
  or a function to be used to determine data indices to keep. If
  `method` is not provided, `"downcast"` is assumed. See “Details”.

- removeDepthInversions:

  Logical value indicating whether to remove any levels at which depth
  is less than, or equal to, a depth above. (This is needed if the
  object is to be assembled into a section, unless
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md)
  will be used, which will remove the inversions.

- parameters:

  A list whose elements depend on the method; see “Details”.

- indices:

  Logical value indicating what to return. If `indices=FALSE` (the
  default), then the return value is a subsetted
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.
  If `indices=TRUE`, then the return value is a logical vector that
  could be used to subset the data with
  [`subset,ctd-method()`](https://dankelley.github.io/oce/reference/subset-ctd-method.md)
  or to set data-quality flags.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

Either a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
object or a logical vector of length matching the data. In the first
case, which is the default, the elements of the `data` slot will have
been trimmed, along with some elements of the `metadata` slot (e.g.
`metadata4flags` and, if present and of length matching `data$pressure`,
both `metadata$longitude` and `metadata$latitude`). The second case,
achieved by setting `indices=FALSE`, may be helpful for advanced users
who wish to do things like construct data flags to be inserted into the
object.

## Details

`ctdTrim` begins by examining the pressure differences between
subsequent samples. If these are all of the same value, then the input
`ctd` object is returned, unaltered. This handles the case of
pressure-binned data. However, if the pressure difference varies, a
variety of approaches are taken to trimming the dataset.

- If `method[1]` is `"downcast"` then an attempt is made to keep only
  data for which the CTD is descending. This is done in stages, with
  variants based on `method[2]`, if supplied.

  1.  The pressure data are despiked with a smooth() filter with method
      "3R". This removes wild spikes that arise from poor instrument
      connections, etc.

  2.  *Step 2.* If no `parameters` are given, then any data with
      negative pressures are deleted. If there is a parameter named
      `pmin`, then that pressure (in decibars) is used instead as the
      lower limit. This is a commonly-used setup, e.g.
      `ctdTrim(ctd, parameters=list(pmin=1))` removes the top decibar
      (roughly 1m) from the data. Specifying `pmin` is a simple way to
      remove near-surface data, such as a shallow equilibration phase,
      and if specified will cause `ctdTrim` to skip step 4 below.

  3.  The maximum pressure is determined, and data acquired subsequent
      to that point are deleted. This removes the upcast and any
      subsequent data.

  4.  If the `pmin` parameter is not specified, an attempt is made to
      remove an initial equilibrium phase by a regression of pressure on
      scan number. There are three variants to this, depending on the
      value of the second `method` element. If `method` is `"A"` (or not
      given), the procedure is to call
      [`nls()`](https://rdrr.io/r/stats/nls.html) to fit a piecewise
      linear model of pressure as a function of scan, in which pressure
      is constant for scan less than a critical value, and then linearly
      varying for with scan. This is meant to handle the common
      situation in which the CTD is held at roughly constant depth
      (typically a metre or so) to equilibrate, before it is lowered
      through the water column. If `method` is `"B"`, the procedure is
      similar, except that the pressure in the surface region is taken
      to be zero (this does not make much sense, but it might help in
      some cases). Note that, prior to early 2016, method `"B"` was
      called method `"C"`; the old `"B"` method was judged useless and
      so it was removed.

- If `method="upcast"`, a sort of reverse of `"downcast"` is used. This
  was added in late April 2017 and has not been well tested yet.

- If `method="sbe"`, a method similar to that described in the SBE Data
  Processing manual is used to remove the "soak" period at the beginning
  of a cast (see Section 6 under subsection "Loop Edit"). The method is
  based on the soak procedure whereby the instrument sits at a fixed
  depth for a period of time, after which it is raised toward the
  surface before beginning the actual downcast. This enables
  equilibration of the sensors while still permitting reasonably good
  near-surface data. Parameters for the method can be passed using the
  `parameters` argument, which include `minSoak` (the minimum depth for
  the soak) and `maxSoak` the maximum depth of the soak. The method
  finds the minimum pressure prior to the `maxSoak` value being passed,
  each of which occurring after the scan in which the `minSoak` value
  was reached. For the method to work, the pre-cast pressure minimum
  must be less than the `minSoak` value. The default values of `minSoak`
  and `maxSoak` are 1 and 20 dbar, respectively.

- If `method="index"` or `"scan"`, then each column of data is subsetted
  according to the value of `parameters`. If the latter is a logical
  vector of length matching data column length, then it is used directly
  for subsetting. If `parameters` is a numerical vector with two
  elements, then the index or scan values that lie between
  `parameters[1]` and `parameters[2]` (inclusive) are used for
  subsetting. The two-element method is probably the most useful, with
  the values being determined by visual inspection of the results of
  [`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md).
  While this may take a minute or two, the analyst should bear in mind
  that a deep-water CTD profile might take 6 hours, corresponding to
  ship-time costs exceeding a week of salary.

- If `method="range"` then data are selected based on the value of the
  column named `parameters$item`. This may be by range or by critical
  value. By range: select values between `parameters$from` (the lower
  limit) and `parameters$to` (the upper limit) By critical value: select
  if the named column exceeds the value. For example,
  `ctd2 <- ctdTrim(ctd, "range", parameters=list(item="scan", from=5))`
  starts at scan number 5 and continues to the end, while
  `ctdTrim(ctd,"range", parameters=list(item="scan", from=5, to=100))`
  also starts at scan 5, but extends only to scan 100.

- If `method` is a function, then it must return a vector of
  [`logical()`](https://rdrr.io/r/base/logical.html) values, computed
  based on two arguments: `data` (a
  [`list()`](https://rdrr.io/r/base/list.html)), and `parameters` as
  supplied to `ctdTrim`. Both `inferWaterDepth` and `removeInversions`
  are ignored in the function case. See “Examples”.

## Historical Note

The subsetting of `longitude` and `latitude` in the `metadata` slot was
introduced on 2022-12-13, for use with
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects
created using
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) on
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) objects
created by using
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md) on
Ruskin files that hold data from RBR CTD instruments linked with
phone/tablet devices equipped with GPS sensors.

## Sample of Usage


    library(oce)
    data(ctdRaw)
    # Example 1: focus on downcast
    plot(ctdTrim(ctdRaw))
    # Example 2: user-supplied function.
    trimByIndex<-function(data, parameters) {
        parameters[1] < data$scan & data$scan < parameters[2]
    }
    trimmed <- ctdTrim(ctdRaw, trimByIndex, parameters=c(130, 380))
    plot(trimmed)

## References

The Seabird CTD instrument is described at
`http://www.seabird.com/products/spec_sheets/19plusdata.htm`.

Seasoft V2: SBE Data Processing, SeaBird Scientific, 05/26/2016

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

Dan Kelley and Clark Richards
