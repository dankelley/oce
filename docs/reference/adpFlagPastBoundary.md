# Flag adp Data Past Water Column Boundary

Flag variables with the same dimension of `v` in an
[adp](https://dankelley.github.io/oce/reference/adp-class.md) object
that are beyond the water column boundary while retaining existing
flags. Currently, this operation can only be performed on
[adp](https://dankelley.github.io/oce/reference/adp-class.md) objects
that contain bottom ranges. Commonly,
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md)
would then be used to remove such data.

## Usage

``` r
adpFlagPastBoundary(
  x = NULL,
  fields = NULL,
  df = 20,
  smoother,
  trim = 0.15,
  good = 1,
  bad = 4,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object containing bottom ranges.

- fields:

  a variable contained within `x` indicating which field to flag. If
  NULL (the default) then `adpFlagPastBoundary()` applies itself to all
  flag fields that have the same dimensionality as `v` in the `data`
  slot.

- df:

  the degrees of freedom to use during the smoothing spline operation.

- smoother:

  a function used to smooth the boundary distance. If this is not given,
  then [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html)
  is called with `df` set equal to the value of `df` given by the user.
  If it is NULL, then no smoothing is done. If it is a function that
  takes 2 arguments and returns a vector of values, then that is used.
  For example, a user might set
  `smoother=function(x, y) smooth.spline(x,y,nknots=length(x)/5)$y` to
  use a smoothing spline with the indicated number of knots.

- trim:

  a scale factor for boundary trimming (see “Details”).

- good:

  number stored in flags to indicate good data.

- bad:

  number stored in flags to indicate bad data.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

`adpFlagPastBoundary` returns an
[adp](https://dankelley.github.io/oce/reference/adp-class.md) object
with flags adjusted in the specified fields if data are beyond the water
column boundary.

## Details

If the object's `oceCoordinate` is `"beam"`, this works by smoothing the
time-dependent bottom ranges (as controlled by the `smoother` and
perhaps the `df` parameters), beam-by-beam. If `oceCoordinate` is
`"enu"`, `"xyz"`, or `"other"`, smoothing is done based on a
time-dependent bottom range averaged across all the beams. Once this is
done, data within distance of \\1-trim\\ multiplied by the bottom range
are flagged as being bad. The default value of `trim` is 0.15, which is
close to the value (0.134) of \\1-cos(angle\*pi/180)\\, with angle=30 as
the beam angle in degrees.

## See also

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

Jaimie Harbin, Clark Richards, and Dan Kelley
