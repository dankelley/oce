# Seawater Density

Compute \\\rho\\, the *in-situ* density of seawater.

## Usage

``` r
swRho(
  salinity,
  temperature = NULL,
  pressure = NULL,
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

*In-situ* density (kg/m\\^3\\).

## Details

If `eos="unesco"`, the density is calculated using the UNESCO equation
of state for seawater (references 1 and 2), and if `eos="gsw"`, the GSW
formulation (references 3 and 4) is used.

## Temperature units

The UNESCO formulae are defined in terms of temperature measured on the
IPTS-68 scale, whereas the replacement GSW formulae are based on the
ITS-90 scale. Prior to the addition of GSW capabilities, the various
`sw*` functions took temperature to be in IPTS-68 units. As GSW
capabilities were added in early 2015, the assumed unit of `temperature`
was taken to be ITS-90. This change means that old code has to be
modified, by replacing e.g. `swRho(S, T, p)` with
`swRho(S, T90fromT68(T), p)`. At typical oceanic values, the difference
between the two scales is a few millidegrees.

## References

1.  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation
    of fundamental properties of seawater. *Unesco Technical Papers in
    Marine Science*, *44*, 53 pp.

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

Related density routines include
[`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md)
(and equivalents at other pressure horizons),
[`swSigmaT()`](https://dankelley.github.io/oce/reference/swSigmaT.md),
and
[`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md).

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
# The numbers in the comments are the check values listed in reference 1;
# note that temperature in that reference was on the T68 scale, but that
# the present function works with the ITS-90 scale, so a conversion
# is required.
swRho(35, T90fromT68(5), 0, eos = "unesco") # 1027.67547
#> [1] 1027.675
swRho(35, T90fromT68(5), 10000, eos = "unesco") # 1069.48914
#> [1] 1069.489
swRho(35, T90fromT68(25), 0, eos = "unesco") # 1023.34306
#> [1] 1023.343
swRho(35, T90fromT68(25), 10000, eos = "unesco") # 1062.53817
#> [1] 1062.538
```
