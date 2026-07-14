# Electrical Conductivity Ratio From Salinity, Temperature and Pressure

Compute electrical conductivity ratio based on salinity, temperature,
and pressure (relative to the conductivity of seawater with salinity=35,
temperature68=15, and pressure=0).

## Usage

``` r
swCSTp(
  salinity,
  temperature = 15,
  pressure = 0,
  eos = getOption("oceEOS", default = "gsw")
)
```

## Arguments

- salinity:

  practical salinity, or a CTD object (in which case its temperature and
  pressure are used, and the next two arguments are ignored)

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  the examples, as well as the “Temperature units” section in the
  documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- pressure:

  pressure (dbar)

- eos:

  equation of state, either `"unesco"` or `"gsw"`.

## Value

Conductivity ratio (unitless), i.e. the ratio of conductivity to the
conductivity at salinity=35, temperature=15 (IPTS-68 scale) and
pressure=0, which has numerical value 42.9140 mS/cm = 4.29140 S/m (see
Culkin and Smith, 1980, in the regression result cited at the bottom of
the left-hand column on page 23).

## Details

If `eos="unesco"`, the calculation is done by a bisection root search on
the UNESCO formula relating salinity to conductivity, temperature, and
pressure (see
[`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md)). If
it is `"gsw"` then the Gibbs-SeaWater formulation is used, via
[`gsw::gsw_C_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_C_from_SP.md).

## References

1.  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation
    of fundamental properties of seawater. *Unesco Technical Papers in
    Marine Science*, *44*, 53 pp.

2.  Culkin, F., and Norman D. Smith, 1980. Determination of the
    concentration of potassium chloride solution having the same
    electrical conductivity, at 15 C and infinite frequency, as standard
    seawater of salinity 35.0000 ppt (Chlorinity 19.37394 ppt). *IEEE
    Journal of Oceanic Engineering*, *5*, pp 22-23.

## See also

For thermal (as opposed to electrical) conductivity, see
[`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md).
For computation of salinity from electrical conductivity, see
[`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md).

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
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
stopifnot(abs(1.0 - swCSTp(35, T90fromT68(15), 0, eos = "unesco")) < 1e-7)
stopifnot(abs(1.0 - swCSTp(34.25045, T90fromT68(15), 2000, eos = "unesco")) < 1e-7)
stopifnot(abs(1.0 - swCSTp(34.25045, T90fromT68(15), 2000, eos = "gsw")) < 1e-7)
```
