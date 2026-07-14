# Show an Item in the metadata Slot of an oce Object

This is a helper function for various `summary` functions.

## Usage

``` r
showMetadataItem(
  object,
  name,
  label = "",
  postlabel = "",
  isdate = FALSE,
  quote = FALSE
)
```

## Arguments

- object:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- name:

  name of item

- label:

  label to print before item

- postlabel:

  label to print after item

- isdate:

  boolean indicating whether the item is a time

- quote:

  boolean indicating whether to enclose the item in quotes

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
showMetadataItem(ctd, "ship", "ship")
#> * shipDivcom3
```
