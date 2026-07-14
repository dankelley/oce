# Seawater Viscosity

Compute viscosity of seawater, in \\Pa\cdot s\\

## Usage

``` r
swViscosity(salinity, temperature)
```

## Arguments

- salinity:

  either salinity (PSU) (in which case `temperature` and `pressure` must
  be provided) *or* a `ctd` object (in which case `salinity`,
  `temperature` and `pressure` are determined from the object, and must
  not be provided in the argument list).

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md), and
  the examples below.

## Value

Viscosity of seawater in \\Pa\cdot s\\. Divide by density to get
kinematic viscosity in \\m^2/s\\.

## Details

If the first argument is a `ctd` object, then salinity, temperature and
pressure values are extracted from it, and used for the calculation.

The result is determined from a regression of the data provided in Table
87 of Dorsey (1940). The fit matches the table to within 0.2 percent at
worst, and with average absolute error of 0.07 percent. The maximum
deviation from the table is one unit in the last decimal place.

No pressure dependence was reported by Dorsey (1940).

## References

N. Ernest Dorsey (1940), *Properties of ordinary Water-substance*,
American Chemical Society Monograph Series. Reinhold Publishing.

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
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
swViscosity(30, 10) # 0.001383779
#> [1] 0.001383779
```
