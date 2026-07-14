# Rename Something in the data slot of an oce Object

Rename an item within the `data` slot of an
[oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
also changing `dataNamesOriginal` in the `metadata` slot, so that the
`[[` accessor will still work with the original name that was stored in
the data. Note that this function does not alter associated flags or
units; to take care of those things, use the newer
[`oceRename()`](https://dankelley.github.io/oce/reference/oceRename.md)
function instead.

## Usage

``` r
oceRenameData(object, old, new, note = "")
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- old:

  character value that matches the name of an item in `object`'s `data`
  slot.

- new:

  character value to be used as the new name that matches the name of an
  item in `object`'s `data` slot. Thus must not be the name of something
  that is already in the `data` slot. If `new` is the same as `old`,
  then the object is returned unaltered.

- note:

  character value that holds an explanation of the reason for the
  change. If this is a string of non-zero length, then this is inserted
  in the processing log of the returned value. If it is `NULL`, then no
  entry is added to the processing log. Otherwise, the processing log
  gets a new item that is constructed from the function call.

## See also

Other things related to the data slot:
[`oceDeleteData()`](https://dankelley.github.io/oce/reference/oceDeleteData.md),
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md),
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
CTD <- oceRenameData(ctd, "salinity", "SALT")
stopifnot(all.equal(ctd[["salinity"]], CTD[["SALT"]]))
stopifnot(all.equal(ctd[["sal00"]], CTD[["SALT"]]))
```
