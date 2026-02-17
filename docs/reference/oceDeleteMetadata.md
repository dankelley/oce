# Delete Something in an oce metadata Slot

Return a copy of the supplied object that lacks the named element in its
`metadata` slot, and that has a note about the deletion in its
processing log.

## Usage

``` r
oceDeleteMetadata(object, name)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  String indicating the name of the item to be deleted.

## See also

Other things related to the metadata slot:
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
[`oceRenameMetadata()`](https://dankelley.github.io/oce/reference/oceRenameMetadata.md),
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)

## Author

Dan Kelley
