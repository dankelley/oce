# Create a Composite of amsr Satellite Data

Form averages for each item in the `data` slot of the supplied objects,
taking into account the bad-data codes.

Items within the `data` slots of the objects that are supplied as
arguments are averaged in a way that makes sense for the object class,
i.e. taking into account the particular bad-data codes of that
particular class.

## Usage

``` r
# S4 method for class 'amsr'
composite(object, ...)
```

## Arguments

- object:

  An [amsr](https://dankelley.github.io/oce/reference/amsr-class.md)
  object.

- ...:

  Other amsr objects.

## Details

If none of the objects has good data at any particular pixel (i.e.
particular latitude and longitude), the resultant will have the bad-data
code of the last item in the argument list. The metadata in the result
are taken directly from the metadata of the final argument, except that
the filename is set to a comma-separated list of the component
filenames.

## See also

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

Other functions that create composite objects:
[`composite()`](https://dankelley.github.io/oce/reference/composite.md),
[`composite,list-method`](https://dankelley.github.io/oce/reference/composite-list-method.md)
