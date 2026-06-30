# Squared Buoyancy Frequency for Seawater

Compute \\N^2\\, the square of the buoyancy frequency for a seawater
profile.

## Usage

``` r
swN2(
  pressure,
  sigmaTheta = NULL,
  derivs,
  df,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- pressure:

  either pressure (dbar) (in which case `sigmaTheta` must be provided)
  *or* an object of class `ctd` object (in which case `sigmaTheta` is
  inferred from the object.

- sigmaTheta:

  Surface-referenced potential density minus 1000 (kg/m\\^3\\).

- derivs:

  optional argument to control how the derivative \\d\sigma\_\theta/dp\\
  is calculated. This may be a character string or a function of two
  arguments. See “Details”.

- df:

  argument passed to
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html) if
  this function is used for smoothing; set to `NA` to prevent smoothing.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  additional argument, passed to
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html), in
  the case that `derivs="smoothing"`. See “Details”.

## Value

Square of buoyancy frequency (\\radian^2/s^2\\).

## Details

Smoothing is often useful prior to computing buoyancy frequency, and so
this may optionally be done with
[`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html), unless
`df=NA`, in which case raw data are used. If `df` is not provided, a
possibly reasonable value computed from an analysis of the profile,
based on the number of pressure levels.

The core of the method involves computing potential density referenced
to median pressure, using the UNESCO-style
[swSigmaTheta](https://dankelley.github.io/oce/reference/swSigmaTheta.md)
function, and then differentiating this with respect to pressure. The
`derivs` argument is used to control how this is done, as follows.

- If `derivs` is not supplied, the action is as though it were given as
  the string `"smoothing"`

- If `derivs` equals `"simple"`, then the derivative of density with
  respect to pressure is calculated as the ratio of first-order
  derivatives of density and pressure, each calculated using
  [`diff()`](https://rdrr.io/r/base/diff.html). (A zero is appended at
  the top level.)

- If `derivs` equals `"smoothing"`, then the processing depends on the
  number of data in the profile, and on whether `df` is given as an
  optional argument. When the number of points exceeds 4, and when `df`
  exceeds 1,
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html) is
  used to calculate smoothing spline representation the variation of
  density as a function of pressure, and derivatives are extracted from
  the spline using `predict`. Otherwise, density is smoothed using
  [`smooth()`](https://rdrr.io/r/stats/smooth.html), and derivatives are
  calculated as with the `"simple"` method.

- If `derivs` is a function taking two arguments (first pressure, then
  density) then that function is called directly to calculate the
  derivative, and no smoothing is done before or after that call.

For precise work, it makes sense to skip `swN2` entirely, choosing
whether, what, and how to smooth based on an understanding of
fundamental principles as well as data practicalities.

## Deprecation Notice

Until 2019 April 11, `swN2` had an argument named `eos`. However, this
did not work as stated, unless the first argument was a `ctd` object.
Besides, the argument name was inherently deceptive, because the UNESCO
scheme does not specify how N2 is to be calculated. Nothing is really
lost by making this change, because the new default is the same as was
previously available with the `eos="unesco"` setup, and the
gsw-formulated estimate of N2 is provided by
[`gsw::gsw_Nsquared()`](http://teos-10.github.io/GSW-R/reference/gsw_Nsquared.md)
in the [gsw](https://CRAN.R-project.org/package=gsw) package.

## See also

The
[`gsw::gsw_Nsquared()`](http://teos-10.github.io/GSW-R/reference/gsw_Nsquared.md)
function of the [gsw](https://CRAN.R-project.org/package=gsw) provides
an alternative to this, as formulated in the GSW system. It has a more
sophisticated treatment of potential density, but it is based on simple
first-difference derivatives, so its results may require smoothing,
depending on the dataset and purpose of the analysis.

Other functions that calculate seawater properties:
[`T68fromT90()`](https://dankelley.github.io/oce/reference/T68fromT90.md),
[`T90fromT48()`](https://dankelley.github.io/oce/reference/T90fromT48.md),
[`T90fromT68()`](https://dankelley.github.io/oce/reference/T90fromT68.md),
[`computableWaterProperties()`](https://dankelley.github.io/oce/reference/computableWaterProperties.md),
[`locationForGsw()`](https://dankelley.github.io/oce/reference/locationForGsw.md),
[`swAbsoluteSalinity()`](https://dankelley.github.io/oce/reference/swAbsoluteSalinity.md),
[`swAlpha()`](https://dankelley.github.io/oce/reference/swAlpha.md),
[`swAlphaOverBeta()`](https://dankelley.github.io/oce/reference/swAlphaOverBeta.md),
[`swBeta()`](https://dankelley.github.io/oce/reference/swBeta.md),
[`swCSTp()`](https://dankelley.github.io/oce/reference/swCSTp.md),
[`swConservativeTemperature()`](https://dankelley.github.io/oce/reference/swConservativeTemperature.md),
[`swDepth()`](https://dankelley.github.io/oce/reference/swDepth.md),
[`swDynamicHeight()`](https://dankelley.github.io/oce/reference/swDynamicHeight.md),
[`swLapseRate()`](https://dankelley.github.io/oce/reference/swLapseRate.md),
[`swPressure()`](https://dankelley.github.io/oce/reference/swPressure.md),
[`swRho()`](https://dankelley.github.io/oce/reference/swRho.md),
[`swRrho()`](https://dankelley.github.io/oce/reference/swRrho.md),
[`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md),
[`swSR()`](https://dankelley.github.io/oce/reference/swSR.md),
[`swSTrho()`](https://dankelley.github.io/oce/reference/swSTrho.md),
[`swSigma()`](https://dankelley.github.io/oce/reference/swSigma.md),
[`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md),
[`swSigma1()`](https://dankelley.github.io/oce/reference/swSigma1.md),
[`swSigma2()`](https://dankelley.github.io/oce/reference/swSigma2.md),
[`swSigma3()`](https://dankelley.github.io/oce/reference/swSigma3.md),
[`swSigma4()`](https://dankelley.github.io/oce/reference/swSigma4.md),
[`swSigmaT()`](https://dankelley.github.io/oce/reference/swSigmaT.md),
[`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md),
[`swSoundAbsorption()`](https://dankelley.github.io/oce/reference/swSoundAbsorption.md),
[`swSoundSpeed()`](https://dankelley.github.io/oce/reference/swSoundSpeed.md),
[`swSpecificHeat()`](https://dankelley.github.io/oce/reference/swSpecificHeat.md),
[`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md),
[`swSpiciness0()`](https://dankelley.github.io/oce/reference/swSpiciness0.md),
[`swSpiciness1()`](https://dankelley.github.io/oce/reference/swSpiciness1.md),
[`swSpiciness2()`](https://dankelley.github.io/oce/reference/swSpiciness2.md),
[`swSstar()`](https://dankelley.github.io/oce/reference/swSstar.md),
[`swTFreeze()`](https://dankelley.github.io/oce/reference/swTFreeze.md),
[`swTSrho()`](https://dankelley.github.io/oce/reference/swTSrho.md),
[`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md),
[`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r

library(oce)
data(ctd)
# Left panel: density
p <- ctd[["pressure"]]
ylim <- rev(range(p))
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))
plot(ctd[["sigmaTheta"]], p, ylim = ylim, type = "l", xlab = expression(sigma[theta]))
# Right panel: N2, with default settings (black) and with df=2 (red)
N2 <- swN2(ctd)
plot(N2, p, ylim = ylim, xlab = "N2 [1/s^2]", ylab = "p", type = "l")
lines(swN2(ctd, df = 3), p, col = 2)

```
