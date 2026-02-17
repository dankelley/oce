# Bin-count Vector Data

Count the number of elements of a given vector that fall within
successive pairs of values within a second vector.

## Usage

``` r
binCount1D(x, xbreaks, include.lowest = FALSE)
```

## Arguments

- x:

  vector of numerical values.

- xbreaks:

  Vector of values of x at the boundaries between bins, calculated using
  [`pretty()`](https://rdrr.io/r/base/pretty.html) if not supplied.

- include.lowest:

  logical value indicating whether to include `x` values that equal
  `xbreaks[1]`. See “Details”.

## Value

A list with the following elements: the breaks (`xbreaks`, midpoints
(`xmids`) between those breaks, and the count (`number`) of `x` values
between successive breaks.

## Details

By default, the sub-intervals defined by the `xbreaks` argument are open
on the left and closed on the right, to match the behaviour of
[`cut()`](https://rdrr.io/r/base/cut.html). An open interval does not
include points on the boundary, and so any `x` values that exactly match
the first `breaks` value will not be counted. To count such points, set
`include.lowest` to TRUE.

To contextualize `binCount1D()` in terms of base R functions, note that

    binCount1D(1:20, seq(0, 20, 2))$number

matches

    unname(table(cut(1:20, seq(0, 20, 2))))

## See also

Other bin-related functions:
[`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md),
[`binApply2D()`](https://dankelley.github.io/oce/reference/binApply2D.md),
[`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md),
[`binCount2D()`](https://dankelley.github.io/oce/reference/binCount2D.md),
[`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md),
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)

## Author

Dan Kelley
