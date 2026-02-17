# Low-Level Function for Handling Data-Quality Flags

This function is designed for internal use within the `oce` package. Its
purpose is to carry out low-level processing relating to data-quality
flags, as a support for higher-level functions such
[handleFlags,ctd-method](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md)
for `ctd` objects,
[handleFlags,adp-method](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md)
for `adp` objects, etc.

## Usage

``` r
handleFlagsInternal(object, flags, actions, where, debug = 0)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- flags:

  a named [list](https://rdrr.io/r/base/list.html) of numeric values.

- actions:

  A character vector indicating actions to be carried out for the
  corresponding `flags` values. This will be lengthened with
  [`rep()`](https://rdrr.io/r/base/rep.html) if necessary, to be of the
  same length as `flags`. A common value for `actions` is `"NA"`, which
  means that data values that are flagged are replaced by `NA` in the
  returned result.

- where:

  An optional string that permits the function to work with objects that
  store flags in e.g. `object@metadata$flags$where` instead of in
  `object@metadata$flags`, and data within `object@data$where` instead
  of within `object@data`. The appropriate value for `where` within the
  oce package is the default, `NULL`, which means that this extra
  subdirectory is not being used.

- debug:

  An integer indicating the degree of debugging requested, with value
  `0` meaning to act silently, and value `1` meaning to print some
  information about the steps in processing.

## Value

A copy of `object`, possibly with modifications to its `data` slot, if
`object` contains flag values that have actions that alter the data.
