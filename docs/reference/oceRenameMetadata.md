# Rename Something in the metadata Slot of an oce Object

Rename an item within the `metadata` slot of an
[oce](https://dankelley.github.io/oce/reference/oce-class.md) object.

## Usage

``` r
oceRenameMetadata(object, old, new, note = "")
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- old:

  character value that matches the name of an item in `object`'s
  `metadata` slot.

- new:

  character value to be used as the new name that matches the name of an
  item in `object`'s `metadata` slot. Thus must not be the name of
  something that is already in the `metadata` slot. If `new` is the same
  as `old`, then the object is returned unaltered.

- note:

  character value that holds an explanation of the reason for the
  change. If this is a string of non-zero length, then this is inserted
  in the processing log of the returned value. If it is `NULL`, then no
  entry is added to the processing log. Otherwise, the processing log
  gets a new item that is constructed from the function call.

## See also

Other things related to the metadata slot:
[`oceDeleteMetadata()`](https://dankelley.github.io/oce/reference/oceDeleteMetadata.md),
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)

## Author

Dan Kelley
