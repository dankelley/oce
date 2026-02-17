# Alter an adp Object to Account for Magnetic Declination

Acoustic-Doppler profiling instruments that infer direction using
magnetic compasses to determine current direction need to have a
correction applied for magnetic declination, if the goal is to infer
currents with x and y oriented eastward and northward, respectively.
This is what the present function does (see “Details”).

## Usage

``` r
# S4 method for class 'adp'
applyMagneticDeclination(
  object = "oce",
  declination = 0,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- declination:

  numeric value holding magnetic declination in degrees, positive for
  clockwise from north.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

An [adp](https://dankelley.github.io/oce/reference/adp-class.md) object,
modified as outlined in “Description”.

## Details

The returned value is a copy of `object` that has been modified in 4
ways. (1) the horizontal components of velocity are rotated clockwise by
`declination` degrees. (2) If the object holds heading values, then
`declination` is added to them. (3) The `north` item in the `metadata`
slot is set to `"geographic"`, and a warning is issued if this was also
the value in `object`. (4) The `declination` item in the `metadata` slot
is set to the value supplied to this function.

## See also

Use
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)
to determine the declination, inclination and intensity at a given spot
on the world, at a given time.

Other things related to magnetism:
[`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`applyMagneticDeclination,oce-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-oce-method.md),
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpConvertRawToNumeric()`](https://dankelley.github.io/oce/reference/adpConvertRawToNumeric.md),
[`adpEnsembleAverage()`](https://dankelley.github.io/oce/reference/adpEnsembleAverage.md),
[`adpFlagPastBoundary()`](https://dankelley.github.io/oce/reference/adpFlagPastBoundary.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`as.adp()`](https://dankelley.github.io/oce/reference/as.adp.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`beamToXyzAdp()`](https://dankelley.github.io/oce/reference/beamToXyzAdp.md),
[`beamToXyzAdpAD2CP()`](https://dankelley.github.io/oce/reference/beamToXyzAdpAD2CP.md),
[`beamToXyzAdv()`](https://dankelley.github.io/oce/reference/beamToXyzAdv.md),
[`beamUnspreadAdp()`](https://dankelley.github.io/oce/reference/beamUnspreadAdp.md),
[`binmapAdp()`](https://dankelley.github.io/oce/reference/binmapAdp.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdp()`](https://dankelley.github.io/oce/reference/enuToOtherAdp.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md),
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subtractBottomVelocity()`](https://dankelley.github.io/oce/reference/subtractBottomVelocity.md),
[`summary,adp-method`](https://dankelley.github.io/oce/reference/summary-adp-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdp()`](https://dankelley.github.io/oce/reference/toEnuAdp.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdp()`](https://dankelley.github.io/oce/reference/xyzToEnuAdp.md),
[`xyzToEnuAdpAD2CP()`](https://dankelley.github.io/oce/reference/xyzToEnuAdpAD2CP.md)

## Author

Dan Kelley, aided by Clark Richards and Jaimie Harbin.

## Examples

``` r
# Transform beam coordinate to xyx, then to enu with respect to
# magnetic north, and then to geographic north.
library(oce)
file <- system.file("extdata", "adp_rdi.000", package = "oce")
lon <- -69.73433
lat <- 47.88126
beam <- read.oce(file, from = 1, to = 4, longitude = lon, latitude = lat)
dec <- magneticField(lon, lat, beam[["time"]][1])$declination
xyz <- beamToXyzAdp(beam)
# Here, we tell xyzToEnuAdp() not to set a declination,
# so enuMag has metadata$north equal to "magnetic".  We could
# also skip the use of applyMagneticDeclination() by supplying
# the known declination to xyzToEnuAdp().
enuMag <- xyzToEnuAdp(xyz, declination = NULL)
enuGeo <- applyMagneticDeclination(enuMag, declination = dec)
```
