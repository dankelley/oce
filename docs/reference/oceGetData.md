# Extract Something From the data Slot of an oce Object

In contrast to the various `[[` functions, this is guaranteed to look
only within the `data` slot. If the named item is not found, `NULL` is
returned.

## Usage

``` r
oceGetData(object, name)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  String indicating the name of the item to be found.

## See also

Other things related to the data slot:
[`oceDeleteData()`](https://dankelley.github.io/oce/reference/oceDeleteData.md),
[`oceRenameData()`](https://dankelley.github.io/oce/reference/oceRenameData.md),
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)

## Author

Dan Kelley
