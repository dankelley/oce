# Find Profiles Within a Tow-Yow ctd Record

Examine the pressure record looking for extended periods of either
ascent or descent, and return either indices to these events or a vector
of CTD records containing the events.

## Usage

``` r
ctdFindProfiles(
  x,
  cutoff = 0.5,
  minLength = 10,
  minHeight,
  smoother = smooth.spline,
  direction = c("descending", "ascending"),
  breaks,
  arr.ind = FALSE,
  distinct,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
  object.

- cutoff:

  criterion on pressure difference; see “Details”. If not provided, this
  defaults to 0.5.

- minLength:

  lower limit on number of points in candidate profiles. If not
  provided, this defaults to 10.

- minHeight:

  lower limit on height of candidate profiles. If not provided, this
  defaults to 0.1 times the pressure span.

- smoother:

  The smoothing function to use for identifying down/up casts. The
  default is `smooth.spline`, which performs well for a small number of
  cycles; see “Examples” for a method that is better for a long tow-yo.
  The return value from `smoother` must be either a list containing an
  element named `y` or something that can be coerced to a vector with
  [`as.vector()`](https://rdrr.io/r/base/vector.html). To turn smoothing
  off, so that cycles in pressure are determined by simple first
  difference, set `smoother` to `NULL`.

- direction:

  String indicating the travel direction to be selected.

- breaks:

  optional integer vector indicating the indices of last datum in each
  profile stored within `x`. Thus, the first profile in the return value
  will contain the `x` data from indices 1 to `breaks[1]`. If `breaks`
  is given, then all other arguments except `x` are ignored. Using
  `breaks` is handy in cases where other schemes fail, or when the
  author has independent knowledge of how the profiles are strung
  together in `x`.

- arr.ind:

  logical value indicating whether the array indices should be returned;
  the alternative is to return a vector of ctd objects.

- distinct:

  An optional string indicating how to identify profiles by unique
  values. Use `"location"` to find profiles by a change in longitude and
  latitude, or use the name of any of item in the `data` slot in `x`. In
  these cases, all the other arguments except `x` are ignored. However,
  if `distinct` is not supplied, the other arguments are handled as
  described above.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  Optional extra arguments that are passed to the smoothing function,
  `smoother`.

## Value

If `arr.ind=TRUE`, a data frame with columns `start` and `end`, the
indices of the downcasts. Otherwise, a vector of `ctd` objects. In this
second case, the station names are set to a form like `"10/3"`, for the
third profile within an original ctd object with station name `"10"`, or
to `"3"`, if the original ctd object had no station name defined.

## Details

The method works by examining the pressure record. First, this is
smoothed using `smoother()` (see “Arguments”), and then the result is
first-differenced using [`diff()`](https://rdrr.io/r/base/diff.html).
Median values of the positive and negative first-difference values are
then multiplied by `cutoff`. This establishes criteria for any given
point to be in an ascending profile, a descending profile, or a
non-profile. Contiguous regions are then found, and those that have
fewer than `minLength` points are discarded. Then, those that have
pressure ranges less than `minHeight` are discarded.

Caution: this method is not well-suited to all datasets. For example,
the default value of `smoother` is
[`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html), and
this works well for just a few profiles, but poorly for a tow-yo with a
long sequence of profiles; in the latter case, it can be preferable to
use simpler smoothers (see “Examples”). Also, depending on the sampling
protocol, it is often necessary to pass the resultant profiles through
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md), to
remove artifacts such as an equilibration phase, etc. Generally, one is
well-advised to use the present function for a quick look at the data,
relying on e.g.
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md) to
identify profiles visually, for a final product.

## Sample of Usage


    library(oce)
    # These examples cannot be tested, because they are based on
    # data objects that are not provided with oce.

    # Example 1. Find profiles within a towyo file, as can result
    # if the CTD is cycled within the water column as the ship
    # moves.
    profiles <- ctdFindProfiles(towyo)

    # Example 2. Use a moving average to smooth pressure, instead of the
    # default smooth.spline() method. This might avoid a tendency of
    # the default scheme to miss some profiles in a long towyo.
    movingAverage <- function(x, n = 11, ...)
    {
        f <- rep(1/n, n)
        stats::filter(x, f, ...)
    }
    casts <- ctdFindProfiles(towyo, smoother=movingAverage)

    # Example 3: glider data read into a ctd object. Chop
    # into profiles by looking for pressure jumps exceeding
    # 10 dbar.
    breaks <- which(diff(gliderAsCtd[["pressure"]]) > 10)
    profiles <- ctdFindProfiles(gliderAsCtd, breaks=breaks)

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
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
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

Dan Kelley and Clark Richards
