# Composite by Averaging Across Data

This is done by calling a specialized version of the function defined in
the given class. In the present version, the objects must inherit from
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md), so the
action is to call
[`composite,amsr-method()`](https://dankelley.github.io/oce/reference/composite-amsr-method.md).

Items within the `data` slots of the objects that are supplied as
arguments are averaged in a way that makes sense for the object class,
i.e. taking into account the particular bad-data codes of that
particular class.

## Usage

``` r
# S4 method for class 'list'
composite(object)
```

## Arguments

- object:

  a [list](https://rdrr.io/r/base/list.html) of
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.

## See also

Other functions that create composite objects:
[`composite()`](https://dankelley.github.io/oce/reference/composite.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md)
