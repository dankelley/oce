# Decimate a ctd Profile

Interpolate a CTD profile to specified pressure values. This is used by
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
but is also useful for dealing with individual CTD/bottle profiles.

## Usage

``` r
ctdDecimate(
  x,
  p = 1,
  method = "boxcar",
  rule = 1,
  e = 1.5,
  na.rm = FALSE,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
  object.

- p:

  pressure increment, or vector of pressures. In the first case,
  pressures from 0dbar to the rounded maximum pressure are used,
  incrementing by `p` dbars. If a vector of pressures is given,
  interpolation is done to these pressures.

- method:

  the method to be used for calculating decimated values. This may be a
  string specifying the method, or a function. In the string case, the
  possibilities are as follows.

  - `"boxcar"` (based on a local average)

  - `"approx"` (based on linear interpolation between neighboring
    points, using [`approx()`](https://rdrr.io/r/stats/approxfun.html)
    with the `rule` argument specified here)

  - `"approxML"` as `"approx"`, except that a mixed layer is assumed to
    apply above the top data value; this is done by setting the `yleft`
    argument to [`approx()`](https://rdrr.io/r/stats/approxfun.html),
    and by calling that function with `rule=c(2, 1))`

  - `"lm"` (based on local regression, with `e` setting the size of the
    local region);

  - `"rr"` for the Reiniger and Ross method, carried out with
    [`oce.approx()`](https://dankelley.github.io/oce/reference/oceApprox.md);

  - `"unesco"` (for the UNESCO method, carried out with
    [`oce.approx()`](https://dankelley.github.io/oce/reference/oceApprox.md).

  On the other hand, if `method` is a function, then it must take two
  arguments, named `data` and `parameters`. The first is set to `x@data`
  by
  [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md).
  The second is passed directly to the user's function (see Example 2).
  The return value from the function must be a logical vector of the
  same length as the `pressure` data, with TRUE values meaning to keep
  the corresponding entries of the `data` slot.

- rule:

  an integer that is passed to
  [`approx()`](https://rdrr.io/r/stats/approxfun.html), in the case
  where `method` is `"approx"`. Note that the default value for `rule`
  is 1, which will inhibit extrapolation beyond the observed pressure
  range. This is a change from the behaviour previous to May 8, 2017,
  when a `rule` of 2 was used (without stating so as an argument).

- e:

  is an expansion coefficient used to calculate the local neighbourhoods
  for the `"boxcar"` and `"lm"` methods. If `e=1`, then the
  neighbourhood for the i-th pressure extends from the (`i-1`)-th
  pressure to the (`i+1`)-th pressure. At the endpoints it is assumed
  that the outside bin is of the same pressure range as the first inside
  bin. For other values of `e`, the neighbourhood is expanded linearly
  in each direction. If the `"lm"` method produces warnings about
  "prediction from a rank-deficient fit", a larger value of `"e"` should
  be used.

- na.rm:

  logical value indicating whether to remove NA values before
  decimating. This value is ignored unless `method` is `boxcar` in which
  case it is passed to
  [`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md)
  which does the averaging. This parameter was added in February 2024,
  and the behaviour of `ctdDecimate()` prior that date was equivalent to
  `na.rm=FALSE`, so that is the default value, even though it is
  expected that many uses will find using TRUE is more convenient. See
  `https://github.com/dankelley/oce/issues/2192` for more discussion.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object,
with pressures that are as set by the `"p"` parameter and all other
properties modified appropriately.

## Details

The `"approx"` and `"approxML"` methods may be best for bottle data, in
which the usual task is to interpolate from a coarse sampling grid to a
finer one. The distinction is that `"approxML"` assumes a mixed-layer
above the top sample value. For CTD data, the `"boxcar"` method may be
the preferred choice, because the task is normally to sub-sample, and
some degree of smoothing is usually desired. (The `"lm"` method can be
quite slow, and its results may be quite similar to those of the boxcar
method.)

For widely-spaced data, a sort of numerical cabbeling effect can result
when density is computed based on interpolated salinity and temperature.
See reference 2 for a discussion of this issue and possible solutions.

## A note about flags

Data-quality flags contained within the original object are ignored by
this function, and the returned value contains no such flags. This is
because such flags represent an assessment of the original data, not of
quantities derived from those data. This function produces a warning to
this effect. The recommended practice is to use
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md)
or some other means to deal with flags before calling the present
function.

## References

1.  R.F. Reiniger and C.K. Ross, 1968. A method of interpolation with
    application to oceanographic data. *Deep Sea Research*, **15**,
    185-193.

2.  Oguma, Sachiko, Toru Suzuki, Yutaka Nagata, Hidetoshi Watanabe,
    Hatsuyo Yamaguchi, and Kimio Hanawa. “Interpolation Scheme for
    Standard Depth Data Applicable for Areas with a Complex
    Hydrographical Structure.” Journal of Atmospheric and Oceanic
    Technology 21, no. 4 (April 1, 2004): 704-15.

## See also

The documentation for
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) explains
the structure of CTD objects, and also outlines the other functions
dealing with them.

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
library(oce)
data(ctd)
plotProfile(ctd, "salinity", ylim = c(10, 0))
p <- seq(0, 45, 1)
ctd2 <- ctdDecimate(ctd, p = p)
lines(ctd2[["salinity"]], ctd2[["pressure"]], col = "blue")
p <- seq(0, 45, 1)
ctd3 <- ctdDecimate(ctd, p = p, method = function(x, y, xout) {
    predict(smooth.spline(x, y, df = 30), xout)$y
})
lines(ctd3[["salinity"]], ctd3[["pressure"]], col = "red")

```
