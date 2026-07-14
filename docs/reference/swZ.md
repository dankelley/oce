# Vertical Coordinate

Compute height above the surface. This is the negative of depth, and so
is defined simply in terms of
[`swDepth()`](https://dankelley.github.io/oce/reference/swDepth.md).

## Usage

``` r
swZ(
  pressure,
  latitude = 45,
  eos = getOption("oceEOS", default = "gsw"),
  debug = getOption("oceDebug")
)
```

## Arguments

- pressure:

  either pressure (dbar), in which case `latitude` must also be given,
  or a `ctd` object, in which case `latitude` will be inferred from the
  object.

- latitude:

  numeric value for latitude in degrees North.

- eos:

  character value indicating the formulation to be used, either
  `"unesco"` or `"gsw"`.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

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
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md)
