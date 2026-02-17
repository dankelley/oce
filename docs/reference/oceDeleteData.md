# Delete Something From the data Slot of an oce Object

Return a copy of the supplied object that lacks the named element in its
`data` slot, and that has a note about the deletion in its processing
log.

## Usage

``` r
oceDeleteData(object, name)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  String indicating the name of the item to be deleted.

## See also

Other things related to the data slot:
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md),
[`oceRenameData()`](https://dankelley.github.io/oce/reference/oceRenameData.md),
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)

## Author

Dan Kelley
