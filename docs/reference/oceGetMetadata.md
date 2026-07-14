# Extract Something From the metadata Slot of an oce Object

In contrast to the various `[[` functions, this is guaranteed to look
only within the `metadata` slot. If the named item is not found, `NULL`
is returned.

## Usage

``` r
oceGetMetadata(object, name)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  String indicating the name of the item to be found.

## See also

Other things related to the metadata slot:
[`oceDeleteMetadata()`](https://dankelley.github.io/oce/reference/oceDeleteMetadata.md),
[`oceRenameMetadata()`](https://dankelley.github.io/oce/reference/oceRenameMetadata.md),
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)

## Author

Dan Kelley
