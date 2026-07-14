# Extract The Start of an Oce Object

Extract The Start of an Oce Object

This function handles the following object classes directly:
[adp](https://dankelley.github.io/oce/reference/adp-class.md),
[adv](https://dankelley.github.io/oce/reference/adv-class.md),
[argo](https://dankelley.github.io/oce/reference/argo-class.md)
(selection by profile),
[coastline](https://dankelley.github.io/oce/reference/coastline-class.md),
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md),
[echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
(selection by ping),
[section](https://dankelley.github.io/oce/reference/section-class.md)
(selection by station) and
[topo](https://dankelley.github.io/oce/reference/topo-class.md)
(selection by longitude and latitude). It does not handle
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) or
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
yet, instead issuing a warning and returning `x` in those cases. For all
other classes, it calls [`head()`](https://rdrr.io/r/utils/head.html)
with `n` as provided, for each item in the `data` slot, issuing a
warning if that item is not a vector; the author is quite aware that
this may not work well for all classes. The plan is to handle all
appropriate classes by July 2018. Please contact the author if there is
a class you need handled before that date.

## Usage

``` r
# S3 method for class 'oce'
head(x, n = 6L, ...)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- n:

  Number of elements to extract, as for
  [`head()`](https://rdrr.io/r/utils/head.html).

- ...:

  ignored

## See also

[`tail.oce()`](https://dankelley.github.io/oce/reference/tail.oce.md),
which yields the end of an `oce` object.

## Author

Dan Kelley
