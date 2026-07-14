# Seawater Thermal Conductivity

Compute seawater thermal conductivity, in \\W m^{-1\circ}C^{-1}\\

## Usage

``` r
swThermalConductivity(salinity, temperature = NULL, pressure = NULL)
```

## Arguments

- salinity:

  salinity (PSU), or a `ctd` object, in which case `temperature` and
  `pressure` will be ignored.

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- pressure:

  pressure (dbar)

## Value

Conductivity of seawater in \\W m^{-1} {^\circ} C^{-1}\\. To calculate
thermal diffusivity in \\m^2/s^2\\, divide by the product of density and
specific heat, as in the example.

## Details

Caldwell's (1974) detailed formulation is used. To be specific, his
equation 6 to calculate K, and his two sentences above that equation are
used to infer this to be K(0,T,S) in his notation of equation 7. Then,
application of his equations 7 and 8 is straightforward. He states an
accuracy for this method of 0.3 percent. (See the check against his
Table 1 in the “Examples”.)

## References

Caldwell, Douglas R., 1974. Thermal conductivity of seawater, *Deep-sea
Research*, *21*, 131-137.

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
[`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Values in m^2/s, a unit that is often used instead of W/(m*degC).
swThermalConductivity(35, 10, 100) / (swRho(35, 10, 100) * swSpecificHeat(35, 10, 100)) # ocean
#> [1] 1.512538e-07
swThermalConductivity(0, 20, 0) / (swRho(0, 20, 0) * swSpecificHeat(0, 20, 0)) # lab
#> [1] 1.526025e-07
# Caldwell Table 1 gives 1478e-6 cal/(cm*sec*degC) at 31.5 o/oo, 10degC, 1kbar
joulePerCalorie <- 4.18400
cmPerM <- 100
swThermalConductivity(31.5, 10, 1000) / joulePerCalorie / cmPerM
#> [1] 0.00147754
```
