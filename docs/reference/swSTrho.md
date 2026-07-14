# Seawater Salinity From Temperature and Density

Compute Practical or Absolute Salinity, given in-situ or Conservative
Temperature, density, and pressure. This is mainly used to draw
isopycnal lines on TS diagrams, hence the dual meanings for salinity and
temperature, depending on the value of `eos`.

## Usage

``` r
swSTrho(
  temperature,
  density,
  pressure,
  eos = getOption("oceEOS", default = "gsw")
)
```

## Arguments

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- density:

  *in-situ* density or sigma value (\\kg/m^3\\)

- pressure:

  *in-situ* pressure (dbar)

- eos:

  equation of state, either `"unesco"` (see references 1 and 2) or
  `"gsw"` (see references 3 and 4).

## Value

Practical Salinity, if `eos="unesco"`, or Absolute Salinity, if
`eos="gsw"`.

## Details

For `eos="unesco"`, finds the practical salinity that yields the given
density, with the given in-situ temperature and pressure. The method is
a bisection search with a salinity tolerance of 0.001. For `eos="gsw"`,
the function
[`gsw::gsw_SA_from_rho()`](http://teos-10.github.io/GSW-R/reference/gsw_SA_from_rho.md)
in the [gsw](https://CRAN.R-project.org/package=gsw) package is used to
infer Absolute Salinity from Conservative Temperature.

## References

1.  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation
    of fundamental properties of seawater. *Unesco Technical Papers in
    Marine Science*, *44*, 53 pp

2.  Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
    York, 662 pp.

3.  IOC, SCOR, and IAPSO (2010). The international thermodynamic
    equation of seawater-2010: Calculation and use of thermodynamic
    properties. Technical Report 56, Intergovernmental Oceanographic
    Commission, Manuals and Guide.

4.  McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10
    and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
    SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.

## See also

[`swTSrho()`](https://dankelley.github.io/oce/reference/swTSrho.md)

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
swSTrho(10, 22, 0, eos = "gsw") # 28.76285
#> [1] 28.76249
swSTrho(10, 22, 0, eos = "unesco") # 28.651625
#> [1] 28.65163
```
