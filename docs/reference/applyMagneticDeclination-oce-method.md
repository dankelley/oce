# Alter an Object to Account for Magnetic Declination

Current-measuring instruments that infer flow direction using magnetic
compasses require a correction for magnetic declination, in order to
infer currents with x and y oriented eastward and northward,
respectively.
[`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md)
is a generic function that handles this task by altering velocity
components (and heading values, if they exist). It works for objects of
the [cm](https://dankelley.github.io/oce/reference/cm-class.md),
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
# S4 method for class 'oce'
applyMagneticDeclination(
  object = "oce",
  declination = 0,
  debug = getOption("oceDebug")
)
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

  a debugging flag, set to a positive value to get debugging.

## Value

an object of the same class as `object`, modified as outlined in
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
[`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md),
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`magneticField()`](https://dankelley.github.io/oce/reference/magneticField.md)

## Author

Dan Kelley, aided, for the
[adp](https://dankelley.github.io/oce/reference/adp-class.md) and
[adv](https://dankelley.github.io/oce/reference/adv-class.md) variants,
by Clark Richards and Jaimie Harbin.
