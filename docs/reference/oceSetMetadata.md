# Set Something in the metadata Slot of an oce Object

Create a copy of an object in which some element of its `metadata` slot
has been altered, or added.

## Usage

``` r
oceSetMetadata(object, name, value, note = "")
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  String indicating the name of the `metadata` item to be set.

- value:

  Value for the item.

- note:

  Either empty (the default), a character string, or `NULL`, to control
  additions made to the processing log of the return value. If `note=""`
  then an entry is created based on deparsing the function call. If
  `note` is a non-empty string, then that string gets added added to the
  processing log. Finally, if `note=NULL`, then nothing is added to the
  processing log. This last form is useful in cases where `oceSetData`
  is to be called many times in succession, resulting in an overly
  verbose processing log; in which case, it might helpful to use
  processingLog\<- to add a summary entry to the object's processing
  log.

## Value

An [oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
the `metadata` slot of which has been altered either by adding a new
item or modifying an existing item.

## See also

Other things related to the metadata slot:
[`oceDeleteMetadata()`](https://dankelley.github.io/oce/reference/oceDeleteMetadata.md),
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
[`oceRenameMetadata()`](https://dankelley.github.io/oce/reference/oceRenameMetadata.md)

## Author

Dan Kelley

## Examples

``` r
# Add an estimate of MLD (mixed layer depth) to a ctd object
library(oce)
data(ctd)
ctdWithMLD <- oceSetMetadata(ctd, "MLD", 3)
ctdWithMLD[["MLD"]] # 3
#> [1] 3
```
