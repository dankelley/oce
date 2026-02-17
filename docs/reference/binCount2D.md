# Bin-count Matrix Data

Count the number of elements of a given matrix z=z(x,y) that fall within
successive pairs of breaks in x and y.

## Usage

``` r
binCount2D(x, y, xbreaks, ybreaks, flatten = FALSE, include.lowest = FALSE)
```

## Arguments

- x, y:

  vectors of numerical values.

- xbreaks, ybreaks:

  vector of values of `x` and `y` at the boundaries between the 2D bins,
  calculated using [`pretty()`](https://rdrr.io/r/base/pretty.html) on
  each of `x` and `y`, if not supplied.

- flatten:

  A logical value indicating whether the return value also contains
  equilength vectors `x`, `y`, `z` and `n`, a flattened representation
  of `xmids`, `ymids`, `result` and `number`.

- include.lowest:

  logical value indicating whether to include points where `x` equals
  `xbreaks[1]` or `y` equals `ybreaks[1]`.

## Value

A list with the following elements: the breaks (`xbreaks` and
`ybreaks`), the midpoints (`xmids` and `ymids`) between those breaks,
and the count (`number`) of `f` values in the boxes defined between
successive breaks.

## Details

By default, the sub-intervals defined by `xbreaks` and `ybreaks` are
open on the left/bottom and closed on the right/top, to match the
behaviour of [`cut()`](https://rdrr.io/r/base/cut.html). An open
interval does not include points on the boundary, and so any `x` and `y`
values that equal `xbreaks[1]` or `ybreaks[1]` will not be counted. To
include such points in the calculation, set `include.lowest` to TRUE.

## See also

Other bin-related functions:
[`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md),
[`binApply2D()`](https://dankelley.github.io/oce/reference/binApply2D.md),
[`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md),
[`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md),
[`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md),
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)

## Author

Dan Kelley
