# Add a Band to a landsat Object

Add a band to a
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
object. Note that it will be stored in numeric form, not raw form, and
therefore it will require much more storage than data read with
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md).

## Usage

``` r
landsatAdd(x, data, name, debug = getOption("oceDebug"))
```

## Arguments

- x:

  a
  [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
  object.

- data:

  A matrix of data, with dimensions matching that of entries already in
  `x`.

- name:

  The name to be used for the data, i.e. the data can later be accessed
  with `d[[name]]` where `d` is the name of the return value from the
  present function.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or a higher value for more debugging.

## Value

A [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
object, with a new data band.

## See also

The documentation for the
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
class explains the structure of landsat objects, and also outlines the
other functions dealing with them.

Other things related to landsat data:
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
`[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

## Author

Dan Kelley
