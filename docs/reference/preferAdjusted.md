# Set Preference for Adjusted Values

[argo](https://dankelley.github.io/oce/reference/argo-class.md) data can
contain "adjusted" forms of data items, which may be more trustworthy
than the original data, and `preferAdjusted` lets the user express a
preference for such adjusted data. This means that using
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
on the results returned by `preferAdjusted` will (if possible) return
adjusted data, and also use those adjusted data in computations of
derived quantities such as Absolute Salinity. The preference applies
also to units and to data-quality flags, both of which can be returned
by
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
as discussed in “Details”.

## Usage

``` r
preferAdjusted(argo, which = "all", fallback = TRUE)
```

## Arguments

- argo:

  An [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  object.

- which:

  A character vector naming the items for which (depending also on the
  value of `fallback`) adjusted values are to be sought by future calls
  to
  [`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md).
  The short names are used, e.g. `which="oxygen"` means that adjusted
  oxygen is to be returned in future calls such as `argo[["oxygen"]]`.
  The default, `"all"`, means to use adjusted values for any item in
  `argo` that has adjusted values.

- fallback:

  A logical value indicating whether to fall back to unadjusted values
  for any data field in which the adjusted values are all `NA`. The
  default value, `TRUE`, avoids a problem with biogeochemical fields,
  where adjustment of any one field may lead to insertion of "adjusted"
  values for other fields that consist of nothing more than `NA`s.

## Value

An [argo](https://dankelley.github.io/oce/reference/argo-class.md)
object its `metadata` slot altered (in its `adjustedWhich` and
`adjustedFallback` elements) as a signal for how
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
should function on the object.

## Details

`preferAdjusted()` merely sets two items in the `metadata` slot of the
returned [argo](https://dankelley.github.io/oce/reference/argo-class.md)
object. The real action is carried out by
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
but, for convenience, the details are explained here.

Consider salinity, for example. If `which` equals `"all"`, or if it is a
character vector containing `"salinity"`, then using
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
on the returned object will yield the adjusted forms of the salinity
data, its associated flags, or its units. Thus, in the salinity case,

- `argo[["salinity"]]` will attempt to return
  `argo@data$salinityAdjusted` instead of returning
  `argo@data$salinity`, although if the adjusted values are all `NA`
  then, depending on the value of `fallback`, the unadjusted values may
  be returned; similarly

- `argo[["salinityFlags"]]` will attempt to return
  `argo@metadata$flags$salinityAdjusted` instead of
  `argo@metadata$flags$salinity`, and

- `argo[["salinityUnits"]]` will attempt to return
  `argo@metadata$units$salinityAdjusted` instead of
  `argo@metadata$units$salinity`.

The default value, `which="all"`, indicates that this preference for
adjusted values will apply to all the elements of the `data` slot of the
returned object, along with associated flags and units. This can be
handy for quick work, but analysts may also choose to restrict their use
of adjusted values to a subset of variables, based on their own
decisions about data quality or accuracy.

The default value `fallback=TRUE` indicates that later calls to
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
should return unadjusted values for any data items that have `NA` for
all the adjusted values. This condition is rare for core variables
(salinity, temperature and pressure) but is annoyingly common for
biogeochemical variables; see e.g. Section 2.2.5 of Reference 1 for a
discussion of the conditions under which Argo NetCDF files contain
adjusted values. Setting `fallback=FALSE` means that adjusted values (if
they exist) will always be returned, even if they are a useless
collection of `NA` values.

Error fields, such as `salinityAdjustedError`, are returned as-is by
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
regardless of whether the object was created by `preferAdjusted`.

It should be noted that, regardless of whether `preferAdjusted` has been
used, the analyst can always access either unadjusted or adjusted data
directly, using the original variable names stored in the source NetCDF
file. For example, `argo[["PSAL"]]` yields unadjusted salinity values,
and `argo[["PSAL_ADJUSTED"]]` yields adjusted values (if they exist, or
`NULL` if they do not). Similarly, adjusted value can always be obtained
by using a form like `argo[["salinityAdjusted"]]`.

## References

1.  Argo Data Management Team. "Argo User's Manual V3.3." Ifremer,
    November 28, 2019.
    [doi:10.13155/29825](https://doi.org/10.13155/29825)

## Author

Dan Kelley, based on discussions with Jaimie Harbin (with respect to the
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md)
interface) and Clark Richards (with respect to storing the preference in
the `metadata` slot).

## Examples

``` r
library(oce)
data(argo)
argoAdjusted <- preferAdjusted(argo)
all.equal(argo[["salinityAdjusted"]], argoAdjusted[["salinity"]])
#> [1] TRUE
all.equal(argo[["salinityFlagsAdjusted"]], argoAdjusted[["salinityFlags"]])
#> [1] TRUE
all.equal(argo[["salinityUnitsAdjusted"]], argoAdjusted[["salinityUnits"]])
#> [1] TRUE
```
