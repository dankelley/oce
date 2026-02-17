# Alter an adv Object to Account for Magnetic Declination

Acoustic-Doppler velocimetry instruments that infer direction using
magnetic compasses need to have a correction applied for magnetic
declination, if the goal is to infer currents with x and y oriented
eastward and northward, respectively. This is what the present function
does (see “Details”).

## Usage

``` r
# S4 method for class 'adv'
applyMagneticDeclination(
  object = "oce",
  declination = 0,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  an [adv](https://dankelley.github.io/oce/reference/adv-class.md)
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

A [adv](https://dankelley.github.io/oce/reference/adv-class.md) object,
adjusted as outlined in “Details”.

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
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`applyMagneticDeclination,oce-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-oce-method.md),
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdv()`](https://dankelley.github.io/oce/reference/enuToOtherAdv.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
[`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md),
[`read.adv.sontek.adr()`](https://dankelley.github.io/oce/reference/read.adv.sontek.adr.md),
[`read.adv.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adv.sontek.serial.md),
[`read.adv.sontek.text()`](https://dankelley.github.io/oce/reference/read.adv.sontek.text.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`summary,adv-method`](https://dankelley.github.io/oce/reference/summary-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md)

## Author

Dan Kelley, aided by Clark Richards and Jaimie Harbin.
