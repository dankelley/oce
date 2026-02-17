# Dynamic Height of a Seawater Profile

Compute the dynamic height of a column of seawater.

## Usage

``` r
swDynamicHeight(
  x,
  referencePressure = 2000,
  subdivisions = 500,
  rel.tol = .Machine$double.eps^0.25,
  eos = getOption("oceEOS", default = "gsw")
)
```

## Arguments

- x:

  a
  [section](https://dankelley.github.io/oce/reference/section-class.md)
  object.

- referencePressure:

  reference pressure (dbar). If this exceeds the highest pressure
  supplied to `swDynamicHeight()`, then that highest pressure is used,
  instead of the supplied value of `referencePressure`.

- subdivisions:

  number of subdivisions for call to
  [`integrate()`](https://rdrr.io/r/stats/integrate.html). (The default
  value is considerably larger than the default for
  [`integrate()`](https://rdrr.io/r/stats/integrate.html), because
  otherwise some test profiles failed to integrate.

- rel.tol:

  absolute tolerance for call to
  [`integrate()`](https://rdrr.io/r/stats/integrate.html). Note that
  this call is made in scaled coordinates, i.e. pressure is divided by
  its maximum value, and dz/dp is also divided by its maximum.

- eos:

  equation of state, either `"unesco"` or `"gsw"`.

## Value

In the first form, a list containing `distance`, the distance (km( from
the first station in the section and `height`, the dynamic height (m).
In the second form, a single value, containing the dynamic height (m).

## Details

If the first argument is a `section`, then dynamic height is calculated
for each station within a section, and returns a list containing
distance along the section along with dynamic height.

If the first argument is a `ctd`, then this returns just a single value,
the dynamic height.

If `eos="unesco"`, processing is as follows. First, a piecewise-linear
model of the density variation with pressure is developed using
[`stats::approxfun()`](https://rdrr.io/r/stats/approxfun.html). (The
option `rule=2` is used to extrapolate the uppermost density up to the
surface, preventing a possible a bias for bottle data, in which the
first depth may be a few metres below the surface.) A second function is
constructed as the density of water with salinity 35PSU, temperature of
0\\^\circ\\C, and pressure as in the `ctd`. The difference of the
reciprocals of these densities, is then integrated with
[`stats::integrate()`](https://rdrr.io/r/stats/integrate.html) with
pressure limits `0` to `referencePressure`. (For improved numerical
results, the variables are scaled before the integration, making both
independent and dependent variables be of order one.)

If `eos="gsw"`,
[`gsw::gsw_geo_strf_dyn_height()`](http://teos-10.github.io/GSW-R/reference/gsw_geo_strf_dyn_height.md)
is used to calculate a result in m^2/s^2, and this is divided by
9.7963\\m/s^2\\. If pressures are out of order, the data are sorted. If
any pressure is repeated, only the first level is used. If there are
under 4 remaining distinct pressures, `NA` is returned, with a warning.

## Sample of Usage

    library(oce)
    data(section)

    # Dynamic height and geostrophy
    par(mfcol=c(2, 2))
    par(mar=c(4.5, 4.5, 2, 1))

    # Left-hand column: whole section
    # (The smoothing lowers Gulf Stream speed greatly)
    westToEast <- subset(section, 1<=stationId&stationId<=123)
    dh <- swDynamicHeight(westToEast)
    plot(dh$distance, dh$height, type="p", xlab="", ylab="dyn. height [m]")
    ok <- !is.na(dh$height)
    smu <- supsmu(dh$distance, dh$height)
    lines(smu, col="blue")
    f <- coriolis(section[["station", 1]][["latitude"]])
    g <- gravity(section[["station", 1]][["latitude"]])
    v <- diff(smu$y)/diff(smu$x) * g / f / 1e3 # 1e3 converts to m
    plot(smu$x[-1], v, type="l", col="blue", xlab="distance [km]", ylab="velocity (m/s)")

    # right-hand column: gulf stream region, unsmoothed
    gs <- subset(section, 102<=stationId&stationId<=124)
    dh.gs <- swDynamicHeight(gs)
    plot(dh.gs$distance, dh.gs$height, type="b", xlab="", ylab="dyn. height [m]")
    v <- diff(dh.gs$height)/diff(dh.gs$distance) * g / f / 1e3
    plot(dh.gs$distance[-1], v, type="l", col="blue",
      xlab="distance [km]", ylab="velocity (m/s)")

## References

Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New York,
662 pp.

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
