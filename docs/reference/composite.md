# Create a Composite Object by Averaging Across Good Data

Items within the `data` slots of the objects that are supplied as
arguments are averaged in a way that makes sense for the object class,
i.e. taking into account the particular bad-data codes of that
particular class.

## Usage

``` r
composite(object, ...)
```

## Arguments

- object:

  either a [list](https://rdrr.io/r/base/list.html) of
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects,
  in which case this is the only argument, or a single
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
  in which case at least one other argument (an object of the same size)
  must be supplied.

- ...:

  Ignored, if `object` is a list. Otherwise, one or more
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects
  of the same sub-class as the first argument.

## See also

Other functions that create composite objects:
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`composite,list-method`](https://dankelley.github.io/oce/reference/composite-list-method.md)
