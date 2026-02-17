# Seawater Freezing Temperature

Compute in-situ freezing temperature of seawater, using either the
UNESCO formulation (computed as in Section 5 of Fofonoff and Millard,
1983) or the GSW formulation (computed by using
[`gsw::gsw_SA_from_SP()`](http://teos-10.github.io/GSW-R/reference/gsw_SA_from_SP.md)
to get Absolute Salinity, and then
[`gsw::gsw_t_freezing()`](http://teos-10.github.io/GSW-R/reference/gsw_t_freezing.md)
to get the freezing temperature).

## Usage

``` r
swTFreeze(
  salinity,
  pressure = NULL,
  longitude = NULL,
  latitude = NULL,
  saturation_fraction = 1,
  eos = getOption("oceEOS", default = "gsw")
)
```

## Arguments

- salinity:

  Either practical salinity (PSU) or a `ctd` object from which practical
  salinity and pressure (plus in the `eos="gsw"` case, longitude and
  latitude) are inferred.

- pressure:

  Seawater pressure (dbar).

- longitude:

  Longitude of observation (only used if `eos="gsw"`; see “Details”).

- latitude:

  Latitude of observation (only used if `eos="gsw"`; see “Details”).

- saturation_fraction:

  The saturation fraction of dissolved air in seawater, ignored if
  `eos="unesco"`).

- eos:

  The equation of state, either `"unesco"` (Fofonoff and Millard, 1983;
  Gill 1982) or `"gsw"` (IOC, SCOR and IAPSO 2010; McDougall and Barker
  2011).

## Value

Temperature (degC), defined on the ITS-90 scale.

## Details

If the first argument is an `oce` object, and if the `pressure` argument
is `NULL`, then the pressure is sought within the first argument. In the
case of `eos="gsw"`, then a similar procedure also applies to the
`longitude` and `latitude` arguments.

## References

Fofonoff, N. P., and R. C. Millard. Algorithms for Computation of
Fundamental Properties of Seawater. UNESCO Technical Papers in Marine
Research. SCOR working group on Evaluation of CTD data;
UNESCO/ICES/SCOR/IAPSO Joint Panel on Oceanographic Tables and
Standards, 1983.

Gill, A E. Atmosphere-Ocean Dynamics. New York, NY, USA: Academic Press,
1982.

IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
seawater-2010: Calculation and use of thermodynamic properties.
Technical Report 56, Intergovernmental Oceanographic Commission, Manuals
and Guide, 2010.

McDougall, Trevor J., and Paul M. Barker. Getting Started with TEOS-10
and the Gibbs Seawater (GSW) Oceanographic Toolbox. SCOR/IAPSO WG127,
2011.

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
[`swTSrho()`](https://dankelley.github.io/oce/reference/swTSrho.md),
[`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md),
[`swTheta()`](https://dankelley.github.io/oce/reference/swTheta.md),
[`swViscosity()`](https://dankelley.github.io/oce/reference/swViscosity.md),
[`swZ()`](https://dankelley.github.io/oce/reference/swZ.md)

## Author

Dan Kelley

## Examples

``` r
# 1. Test for a check-value given in reference 1. This value, -2.588567 degC,
# is in the 1968 temperature scale (IPTS-68), but swTFreeze reports
# in the newer ITS-90 scale, so we must convert before checking.
Tcheck <- -2.588567 # IPTS-68
T <- swTFreeze(salinity = 40, pressure = 500, eos = "unesco")
stopifnot(abs(Tcheck - T68fromT90(T)) < 1e-6)

# 2. Compare unesco and gsw formulations.
data(ctd)
p <- ctd[["pressure"]]
par(mfrow = c(1, 2), mar = c(3, 3, 1, 2), mgp = c(2, 0.7, 0))
plot(swTFreeze(ctd, eos = "unesco"),
    p,
    xlab = "unesco", ylim = rev(range(p))
)
plot(swTFreeze(ctd, eos = "unesco") - swTFreeze(ctd, eos = "gsw"),
    p,
    xlab = "unesco-gsw", ylim = rev(range(p))
)

```
