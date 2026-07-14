# Seawater Potential Density Anomaly

Compute the potential density (minus 1000 kg/m^3) that seawater would
have if raised adiabatically to the surface. In the UNESCO system, this
quantity is is denoted \\\sigma\_\theta\\ (hence the function name), but
in the GSW system, a somewhat related quantity is denoted `sigma0`. (In
a deep-water CTD cast, the RMS deviation between sigma-theta and sigma0
is typically of order 0.0003 kg/m^3, corresponding to a temperature
shift of about 0.002C, so the distinction between the quantities is not
large.)

## Usage

``` r
swSigmaTheta(
  salinity,
  temperature = NULL,
  pressure = NULL,
  referencePressure = 0,
  longitude = NULL,
  latitude = NULL,
  eos = getOption("oceEOS", default = "gsw"),
  debug = getOption("oceDebug")
)
```

## Arguments

- salinity:

  either practical salinity (in which case `temperature` and `pressure`
  must be provided) *or* an `oce` object, in which case `salinity`,
  `temperature` (in the ITS-90 scale; see next item), etc. are inferred
  from the object, ignoring the other parameters, if they are supplied.

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale. This
  scale is used by GSW-style calculation (as requested by setting
  `eos="gsw"`), and is the value contained within `ctd` objects (and
  probably most other objects created with data acquired in the past
  decade or two). Since the UNESCO-style calculation is based on
  IPTS-68, the temperature is converted within the present function,
  using
  [`T68fromT90()`](https://dankelley.github.io/oce/reference/T68fromT90.md).

- pressure:

  pressure (dbar)

- referencePressure:

  The reference pressure, in dbar.

- longitude:

  longitude of observation (only used if `eos="gsw"`; see “Details”).

- latitude:

  latitude of observation (only used if `eos="gsw"`; see “Details”).

- eos:

  equation of state, either `"unesco"` (references 1 and 2) or `"gsw"`
  (references 3 and 4).

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

Potential density anomaly (kg/m\\^3\\), defined as
\\\sigma\_\theta=\rho(S,\theta(S,t,p),0\\

- 1000 kg/m\\^3\\.

## Details

If the first argument is an `oce` object, then salinity, etc., are
extracted from it, and used for the calculation instead of any values
provided in the other arguments.

## References

See citations provided in the
[`swRho()`](https://dankelley.github.io/oce/reference/swRho.md)
documentation.

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
stopifnot(abs(26.4212790994 - swSigmaTheta(35, 13, 1000, eos = "unesco")) < 1e-7)
```
