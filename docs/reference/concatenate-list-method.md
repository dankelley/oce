# Concatenate a List of oce Objects

Concatenate a List of oce Objects

## Usage

``` r
# S4 method for class 'list'
concatenate(object, debug = getOption("oceDebug"))
```

## Arguments

- object:

  a [list](https://rdrr.io/r/base/list.html) of
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects,
  all of which must have the same sub-class (e.g. all of
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md), or
  [adp](https://dankelley.github.io/oce/reference/adp-class.md), etc).

- debug:

  a debugging flag, set to a positive value to get debugging. Note that
  `debug-1` is passed to the other
  [`concatenate()`](https://dankelley.github.io/oce/reference/concatenate.md)
  functions that are called by the present function.

## Value

An object of class corresponding to that in the elements of `object`.

## See also

Other functions for concatenating oce objects:
[`concatenate()`](https://dankelley.github.io/oce/reference/concatenate.md),
[`concatenate,adp-method`](https://dankelley.github.io/oce/reference/concatenate-adp-method.md)
