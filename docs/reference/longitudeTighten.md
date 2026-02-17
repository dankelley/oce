# Try to Reduce Section Longitude Range

longitudeTighten shifts some longitudes in its first argument by 360
degrees, if doing so will reduce the overall longitude span.

## Usage

``` r
longitudeTighten(section)
```

## Arguments

- section:

  a
  [section](https://dankelley.github.io/oce/reference/section-class.md)
  object.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object based on its first argument, but with longitudes shifted in its
`metadata` slot, and also in the `metadata` slots of each of the
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects
that are stored in the `station` item in its `data` slot.

## Details

This function can be helpful in cases where the CTD stations within a
section cross the cut point of the longitude convention, which otherwise
might yield ugly plots if
[`plot,section-method()`](https://dankelley.github.io/oce/reference/plot-section-method.md)
is used with `xtype="longitude"`. This problem does occur with CTD
objects ordered by time of sampling, but was observed in December 2020
for a GO-SHIPS dataset downloaded from
`https://cchdo.ucsd.edu/data/15757/a10_1992_ct1`.

## Author

Dan Kelley
