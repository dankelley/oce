# Alter an Object to Account for Magnetic Declination (Generic)

Current-measuring instruments that infer flow direction using magnetic
compasses require a correction for magnetic declination, in order to
infer currents with x and y oriented eastward and northward,
respectively. `applyMagneticDeclination()` is a generic function that
handles this task by altering velocity components (and heading values,
if they exist). It works for objects of the
[cm](https://dankelley.github.io/oce/reference/cm-class.md),
[adp](https://dankelley.github.io/oce/reference/adp-class.md) and
[adv](https://dankelley.github.io/oce/reference/adv-class.md) and
[cm](https://dankelley.github.io/oce/reference/cm-class.md) classes by
calling
[`applyMagneticDeclination,adp-method()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`applyMagneticDeclination,adv-method()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
or
[`applyMagneticDeclination,cm-method()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
respectively.

## Usage

``` r
applyMagneticDeclination(object = "oce", declination = "ANY", debug = "ANY")
```

## Arguments

- object:

  an object of
  [cm](https://dankelley.github.io/oce/reference/cm-class.md),
  [adp](https://dankelley.github.io/oce/reference/adp-class.md), or
  [adv](https://dankelley.github.io/oce/reference/adv-class.md) class.

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

an object of the same class as `object`, modified as described in
“Details”.

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
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`applyMagneticDeclination,oce-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-oce-method.md),
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)

## Author

Dan Kelley, aided, for the
[adp](https://dankelley.github.io/oce/reference/adp-class.md) and
[adv](https://dankelley.github.io/oce/reference/adv-class.md) variants,
by Clark Richards and Jaimie Harbin.
