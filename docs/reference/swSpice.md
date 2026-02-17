# Seawater Spiciness

Compute seawater "spice", a variable that is in some sense orthogonal to
density in TS space. Larger spice values correspond to relative warm and
salty water, compared with smaller spice values. Two distinct variants
exist. If `eos="unesco"` then Flament's (2002) formulation is used. If
`eos="gsw"` then
[`gsw::gsw_spiciness0()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness0.md)
is used to compute a newer variant that is part of the Gibbs SeaWater
formulation (McDougall and Krzysik, 2015). See the “Examples” section
for a graphical illustration of the difference in a typical coastal
case.

## Usage

``` r
swSpice(
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

  either salinity (PSU) (in which case `temperature` and `pressure` must
  be provided) *or* a `ctd` object (in which case `salinity`,
  `temperature` and `pressure` are determined from the object, and must
  not be provided in the argument list).

- temperature:

  *in-situ* temperature (\\^\circ\\C) on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- pressure:

  Seawater pressure (dbar) (only used if `eos` is `"gsw"`); see
  “Details”..

- longitude:

  longitude of observation (only used if `eos` is `"gsw"`; see
  “Details”).

- latitude:

  latitude of observation (only used if `eos` is `"gsw"`; see
  “Details”).

- eos:

  Character value specifying the equation of state, either `"unesco"`
  (for the Flament formulation, although this is not actually part of
  UNESCO) or `"gsw"` for the Gibbs SeaWater formulation.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

Flament-formulated spice \\kg/m^3\\ if `eos` is `"unesco"` or
surface-referenced GSW spiciness0 \\kg/m^3\\ if `eos` is `"gsw"`, the
latter provided by
[`gsw::gsw_spiciness0()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness0.md),
and hence aimed at application within the top half-kilometre of the
ocean.

## Details

If the first argument is a `ctd` object, then salinity, temperature and
pressure values are extracted from it, and used for the calculation. For
the `eos="gsw"` case, longitude and latitude are also extracted, because
these are required by
[`gsw::gsw_spiciness0()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness0.md).

Roughly speaking, seawater with a high spiciness is relatively warm and
salty compared with less spicy water. Another interpretation is that
spice is a variable measuring distance orthogonal to isopycnal lines on
TS diagrams (if the diagrams are scaled to make the isopycnals run at 45
degrees). Note that pressure, longitude and latitude are all ignored in
the Flament definition.

## References

1.  Flament, P. “A State Variable for Characterizing Water Masses and
    Their Diffusive Stability: Spiciness.” Progress in Oceanography,
    Observations of the 1997-98 El Nino along the West Coast of North
    America, 54, no. 1 (July 1, 2002):493-501.
    [doi:10.1016/S0079-6611(02)00065-4](https://doi.org/10.1016/S0079-6611%2802%2900065-4)

2.  McDougall, Trevor J., and Oliver A. Krzysik. “Spiciness.” Journal of
    Marine Research 73, no. 5 (September 1, 2015): 141-52.

## See also

Other functions that calculate seawater spiciness:
[`swSpiciness0()`](https://dankelley.github.io/oce/reference/swSpiciness0.md),
[`swSpiciness1()`](https://dankelley.github.io/oce/reference/swSpiciness1.md),
[`swSpiciness2()`](https://dankelley.github.io/oce/reference/swSpiciness2.md)

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

Dan Kelley coded this, merely an interface to the code described by
references 1 and 2.

## Examples

``` r
# Compare unesco and gsw formulations
library(oce)
data(ctd)
p <- ctd[["pressure"]]
U <- swSpice(ctd, eos = "unesco")
G <- swSpice(ctd, eos = "gsw")
xlim <- range(c(U, G), na.rm = TRUE)
ylim <- rev(range(p))
plot(U, p,
    xlim = xlim, ylim = ylim,
    xlab = "Measure of Spiciness", ylab = "Pressure (dbar)"
)
points(G, p, col = 2)
legend("topleft", col = 1:2, pch = 1, legend = c("unesco", "gsw"))

```
