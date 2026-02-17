# Seawater Potential Temperature (UNESCO Version)

Compute the potential temperature of seawater, denoted \\\theta\\ in the
UNESCO system, and `pt` in the GSW system.

## Usage

``` r
swTheta(
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

  either salinity (PSU) (in which case `temperature` and `pressure` must
  be provided) *or* an `oce` object (in which case `salinity`, etc. are
  inferred from the object).

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md), and
  the examples below.

- pressure:

  pressure (dbar)

- referencePressure:

  reference pressure (dbar)

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

Potential temperature (\\^\circ\\C) of seawater, referenced to pressure
`referencePressure`.

## Details

Different formulae are used depending on the equation of state. If `eos`
is `"unesco"`, the method of Fofonoff *et al.* (1983) is used (see
references 1 and 2). Otherwise, `swTheta` uses
[`gsw::gsw_pt_from_t()`](http://teos-10.github.io/GSW-R/reference/gsw_pt_from_t.md)
from the [gsw](https://CRAN.R-project.org/package=gsw) package.

If the first argument is a `ctd` or `section` object, then values for
salinity, etc., are extracted from it, and used for the calculation, and
the corresponding arguments to the present function are ignored.

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
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Example 1: test value from Fofonoff et al., 1983
stopifnot(abs(36.8818748026 - swTheta(40, T90fromT68(40), 10000, 0, eos = "unesco")) < 0.0000000001)

# Example 2: a deep-water station. Note that theta and CT are
# visually identical on this scale.
data(section)
stn <- section[["station", 70]]
plotProfile(stn, "temperature", ylim = c(6000, 1000))
lines(stn[["theta"]], stn[["pressure"]], col = 2)
lines(stn[["CT"]], stn[["pressure"]], col = 4, lty = 2)
legend("bottomright",
    lwd = 1, col = c(1, 2, 4), lty = c(1, 1, 2),
    legend = c("in-situ", "theta", "CT"),
    title = sprintf("MAD(theta-CT)=%.4f", mean(abs(stn[["theta"]] - stn[["CT"]])))
)

```
