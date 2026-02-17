# Alter a cm Object to Account for Magnetic Declination

Current-meter (`cm`) instruments determine directions from onboard
compasses, so interpreting velocity components in geographical
coordinates requires that magnetic declination be taken into account.
This is what the present function does (see “Details”).

## Usage

``` r
# S4 method for class 'cm'
applyMagneticDeclination(
  object = "oce",
  declination = 0,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  a [cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

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

A [cm](https://dankelley.github.io/oce/reference/cm-class.md) object,
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
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`applyMagneticDeclination,oce-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-oce-method.md),
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

## Author

Dan Kelley
