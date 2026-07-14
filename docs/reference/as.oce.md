# Coerce Something Into an oce Object

Coerce Something Into an oce Object

## Usage

``` r
as.oce(x, ...)
```

## Arguments

- x:

  an item containing data. This may be data frame, list, or an oce
  object.

- ...:

  optional extra arguments, passed to conversion functions
  [`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md)
  or
  [`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
  if these are used.

## Value

An [oce](https://dankelley.github.io/oce/reference/oce-class.md) object.

## Details

This function is limited and not intended for common use. In most
circumstances, users should employ a function such as
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) to
construct specialized oce sub-classes.

`as.oce` creates an oce object from data contained within its first
argument, which may be a list, a data frame, or an object of
[oce](https://dankelley.github.io/oce/reference/oce-class.md). (In the
last case, `x` is simply returned, without modification.)

If `x` is a list containing items named `longitude` and `latitude`, then
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md)
is called (with the specified ... value) to create a coastline object.

If `x` is a list created by `read_odf()` from the (as yet unreleased)
ODF package developed by the Bedford Institute of Oceanography, then
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md) is
called (with no arguments other than the first) to calculate a return
value. If the sub-class inference made by
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md) is
incorrect, users should call that function directly, specifying a value
for its `coerce` argument.

If `x` has not been created by `read_odf()`, then the names of the items
it contains are examined, and used to try to infer the proper return
value. There are only a few cases (although more may be added if there
is sufficient user demand). The cases are as follows.

- If `x` contains items named `temperature`, `pressure` and either
  `salinity` or `conductivity`, then an object of type
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) will be
  returned.

- If `x` contains columns named `longitude` and `latitude`, but no other
  columns, then an object of class
  [coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
  is returned.
