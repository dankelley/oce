# Concatenate oce Objects (Generic)

Concatenate oce Objects (Generic)

## Usage

``` r
concatenate(object, ..., debug = getOption("oceDebug"))
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- ...:

  optional additional
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.

- debug:

  integer indicating a debugging level. If this is 0, the work is done
  silently. If it is a larger integer, some information may be printed
  during the processing.

## Value

An object of class corresponding to that of `object`.

## See also

Other functions for concatenating oce objects:
[`concatenate,adp-method`](https://dankelley.github.io/oce/reference/concatenate-adp-method.md),
[`concatenate,list-method`](https://dankelley.github.io/oce/reference/concatenate-list-method.md)
