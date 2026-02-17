# Density Ratio

Compute density ratio for a `ctd` object. An error (perhaps with some
hints) is issued for any other type of object.

## Usage

``` r
swRrho(
  ctd,
  sense = c("diffusive", "finger"),
  smoothingLength = 10,
  df,
  eos = getOption("oceEOS", default = "gsw"),
  debug = getOption("oceDebug")
)
```

## Arguments

- ctd:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object that holds `salinity`, `temperature`, and `pressure`. If `eos`
  is `"gsw"`, then it must also hold `longitude` and `latitude`.

- sense:

  an indication of the sense of double diffusion under study and
  therefore of the definition of Rrho; see “Details”

- smoothingLength:

  ignored if `df` supplied, but otherwise the latter is calculated as
  the number of data points, divided by the number within a depth
  interval of `smoothingLength` metres.

- df:

  if given, this is provided to
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html).

- eos:

  equation of state, either `"unesco"` or `"gsw"`.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

Density ratio defined in either the `"diffusive"` or `"finger"` sense.

## Details

If `eos="unesco"`, the work is done by calculating salinity and
potential-temperature derivatives from smoothing splines whose
properties are governed by `smoothingLength` or `df`. If
`sense="diffusive"` the definition is
\\(beta\*dS/dz)/(alpha\*d(theta)/dz)\\ and the reciprocal for
`"finger"`.

If `eos="gsw"`, the work is done by extracting absolute salinity and
conservative temperature, smoothing with a smoothing spline as in the
`"unesco"` case, and then calling
[`gsw::gsw_Turner_Rsubrho()`](http://teos-10.github.io/GSW-R/reference/gsw_Turner_Rsubrho.md)
on these smoothed fields. Since the gsw function works on mid-point
pressures, [`approx()`](https://rdrr.io/r/stats/approxfun.html) is used
to interpolate back to the original pressures.

If the default arguments are acceptable, `ctd[["Rrho"]]` may be used
instead of `swRrho(ctd)`.

## See also

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
[`swN2()`](https://dankelley.github.io/oce/reference/swN2.md),
[`swPressure()`](https://dankelley.github.io/oce/reference/swPressure.md),
[`swRho()`](https://dankelley.github.io/oce/reference/swRho.md),
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

Dan Kelley and Chantelle Layton

## Examples

``` r
library(oce)
data(ctd)
u <- swRrho(ctd, eos = "unesco")
g <- swRrho(ctd, eos = "gsw")
p <- ctd[["p"]]
plot(u, p, ylim = rev(range(p)), type = "l", xlab = expression(R[rho]))
lines(g, p, lty = 2, col = "red")
legend("topright", lty = 1:2, legend = c("unesco", "gsw"), col = c("black", "red"))

```
