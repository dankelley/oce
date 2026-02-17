# Create and Initialize adp Flags

This function creates an item for a named variable within the `flags`
entry in the object's `metadata` slot. The purpose is both to document a
flag scheme and to make it so that
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md)
and
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md)
can specify flags by name, in addition to number. A generic function, it
is specialized for some classes via interpretation of the `scheme`
argument (see “Details”, for those object classes that have such
specializations).

## Usage

``` r
# S4 method for class 'adp'
initializeFlags(
  object,
  name = NULL,
  value = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  An [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  Character value indicating the name of a variable within the `data`
  slot of `object`.

- value:

  Numerical or character value to be stored in the newly-created entry
  within `flags`. (A character value will only work if
  [`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md)
  has been used first on `object`.)

- debug:

  Integer set to 0 for quiet action or to 1 for some debugging.

## Value

An object with the `flags` item within the `metadata` slot set up as
indicated.

## Details

If `object` already contains a `flags` entry with the indicated name,
then it is returned unaltered, and a warning is issued.

## See also

Other functions relating to data-quality flags:
[`defaultFlags()`](https://dankelley.github.io/oce/reference/defaultFlags.md),
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`handleFlags,oce-method`](https://dankelley.github.io/oce/reference/handleFlags-oce-method.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme()`](https://dankelley.github.io/oce/reference/initializeFlagScheme.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`initializeFlagScheme,oce-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-oce-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`initializeFlagSchemeInternal()`](https://dankelley.github.io/oce/reference/initializeFlagSchemeInternal.md),
[`initializeFlags()`](https://dankelley.github.io/oce/reference/initializeFlags.md),
[`initializeFlags,oce-method`](https://dankelley.github.io/oce/reference/initializeFlags-oce-method.md),
[`initializeFlagsInternal()`](https://dankelley.github.io/oce/reference/initializeFlagsInternal.md),
[`setFlags()`](https://dankelley.github.io/oce/reference/setFlags.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`setFlags,oce-method`](https://dankelley.github.io/oce/reference/setFlags-oce-method.md)
