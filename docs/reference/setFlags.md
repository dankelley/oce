# Set Data-Quality Flags within a oce Object

This function changes specified entries in the data-quality flags of a
oce object, which are stored within a list named `flags` that resides in
the `metadata` slot. If the object already has a flag set up for `name`,
then only the specified entries are altered. If not, the flag entry is
first created and its entries set to `default`, after which the entries
specified by `i` are changed to `value`.

The specification is made with `i`, the form of which is determined by
the data item in question. Generally, the rules are as follows:

1.  If the data item is a vector, then `i` must be (a) an integer vector
    specifying indices to be set to `value`, (b) a logical vector of
    length matching the data item, with `TRUE` meaning to set the flag
    to `value`, or (c) a function that takes an `oce` object as its
    single argument, and returns a vector in either of the forms just
    described.

2.  If the data item is an array, then `i` must be (a) a data frame of
    integers whose rows specify spots to change (where the number of
    columns matches the number of dimensions of the data item), (b) a
    logical array that has dimension equal to that of the data item,
    or (c) a function that takes an `oce` object as its single input and
    returns such a data frame or array.

See “Details” for the particular case of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.

## Usage

``` r
setFlags(object, name = NULL, i = NULL, value = NULL, debug = 0)
```

## Arguments

- object:

  An oce object.

- name:

  Character string indicating the name of the variable to be flagged. If
  this variable is not contained in the object's `data` slot, an error
  is reported.

- i:

  Indication of where to insert the flags; see “Description” for general
  rules and “Details” for rules for
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.

- value:

  The value to be inserted in the flag.

- debug:

  Integer set to 0 for quiet action or to 1 for some debugging.

## Value

An object with flags set as indicated.

## Details

This generic function is overridden by specialized functions for some
object classes.

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
[`initializeFlags,adp-method`](https://dankelley.github.io/oce/reference/initializeFlags-adp-method.md),
[`initializeFlags,oce-method`](https://dankelley.github.io/oce/reference/initializeFlags-oce-method.md),
[`initializeFlagsInternal()`](https://dankelley.github.io/oce/reference/initializeFlagsInternal.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`setFlags,oce-method`](https://dankelley.github.io/oce/reference/setFlags-oce-method.md)
