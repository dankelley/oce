# Practical Salinity From Electrical Conductivity, Temperature and Pressure

Calculate salinity from what is actually measured by a CTD, *i.e.*
conductivity, *in-situ* temperature and pressure. Often this is done by
the CTD processing software, but sometimes it is helpful to do this
directly, *e.g.* when there is a concern about mismatches in sensor
response times.

## Usage

``` r
swSCTp(
  conductivity,
  temperature = NULL,
  pressure = NULL,
  conductivityUnit,
  eos = getOption("oceEOS", default = "gsw")
)
```

## Arguments

- conductivity:

  a measure of conductivity (see also `conductivityUnit`) or an `oce`
  object holding hydrographic information. In the second case, all the
  other arguments to `swSCTp` are ignored.

- temperature:

  *in-situ* temperature (\\^\circ\\C), defined on the ITS-90 scale; see
  “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- pressure:

  pressure (dbar).

- conductivityUnit:

  string indicating the unit used for conductivity. This may be
  `"ratio"` or `""` (meaning conductivity ratio), `"mS/cm"` or `"S/m"`.
  Note that the ratio mode assumes that measured conductivity has been
  divided by the standard conductivity of 4.2914 S/m. In dealing with
  unfamiliar data for which the measurement unit has not been recorded,
  it can be sensible to try all three possibilities for
  `conductivityUnit`, to see which yields the most sensible salinities.

- eos:

  equation of state, either `"unesco"` or `"gsw"`.

## Value

Practical Salinity.

## Details

Two variants are provided. First, if `eos` is `"unesco"`, then salinity
is calculated using the UNESCO algorithm described by Fofonoff and
Millard (1983) as in reference 1. Second, if `eos` is `"gsw"`, then the
Gibbs-SeaWater formulation is used, via
[`gsw::gsw_SP_from_C()`](http://teos-10.github.io/GSW-R/reference/gsw_SP_from_C.md)
in the [gsw](https://CRAN.R-project.org/package=gsw) package. The latter
starts with the same formula as the former, but if this yields a
Practical Salinity less than 2, then the result is instead calculated
using formulae provided by Hill et al. (1986; reference 2), modified to
match the `"unesco"` value at Practical salinity equal to 2 (reference
3).

## References

1.  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation
    of fundamental properties of seawater. *Unesco Technical Papers in
    Marine Science*, *44*, 53 pp.

2.  K. Hill, T. Dauphinee, and D. Woods. “The Extension of the Practical
    Salinity Scale 1978 to Low Salinities.” IEEE Journal of Oceanic
    Engineering 11, no. 1 (January 1986): 109-12.
    [doi:10.1109/JOE.1986.1145154](https://doi.org/10.1109/JOE.1986.1145154)

3.  `gsw_from_SP` online documentation, available at
    `http://www.teos-10.org/pubs/gsw/html/gsw_C_from_SP.html`

## See also

For thermal (as opposed to electrical) conductivity, see
[`swThermalConductivity()`](https://dankelley.github.io/oce/reference/swThermalConductivity.md).
For computation of electrical conductivity from salinity, see
[`swCSTp()`](https://dankelley.github.io/oce/reference/swCSTp.md).

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
# 1. Demonstrate agreement with test value in UNESCO documents
swSCTp(1, T90fromT68(15), 0, eos = "unesco") # expect 35
#> [1] 35
# 2. Demonstrate agreement of gsw and unesco, S>2 case
swSCTp(1, T90fromT68(15), 0, eos = "gsw") # again, expect 35
#> [1] 35
# 3. Demonstrate close values even in very brackish water
swSCTp(0.02, 10, 100, eos = "gsw") # 0.6013981
#> [1] 0.6013981
swSCTp(0.02, 10, 100, eos = "unesco") # 0.6011721
#> [1] 0.6011721
```
