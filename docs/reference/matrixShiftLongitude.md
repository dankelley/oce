# Rearrange Areal Matrix so Greenwich is Near the Centre

Sometimes datasets are provided in matrix form, with first index
corresponding to longitudes ranging from 0 to 360.
`matrixShiftLongitude` cuts such matrices at longitude=180, and swaps
the pieces so that the dateline is at the left of the matrix, not in the
middle.

## Usage

``` r
matrixShiftLongitude(m, longitude)
```

## Arguments

- m:

  The matrix to be modified.

- longitude:

  A vector containing the longitude in the 0-360 convention. If missing,
  this is constructed to range from 0 to 360, with as many elements as
  the first index of `m`.

## Value

A list containing `m` and `longitude`, both rearranged as appropriate.

## See also

[`shiftLongitude()`](https://dankelley.github.io/oce/reference/shiftLongitude.md)
and
[`standardizeLongitude()`](https://dankelley.github.io/oce/reference/standardizeLongitude.md).
