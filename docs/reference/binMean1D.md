# Bin-average f=f(x)

Average the values of a vector `f` in bins defined on another vector
`x`. The values are broken up into bins using
[`cut()`](https://rdrr.io/r/base/cut.html).

## Usage

``` r
binMean1D(x, f, xbreaks, include.lowest = FALSE, na.rm = FALSE)
```

## Arguments

- x:

  vector of numerical values that will be categorized into bins via the
  `xbreaks` parameter.

- f:

  vector of numerical values that are associated with the `x` values.

- xbreaks:

  vector of values of `x` at the boundaries between bins, calculated
  using [`pretty()`](https://rdrr.io/r/base/pretty.html) if not
  supplied.

- include.lowest:

  logical value indicating whether to include `x` values that equal
  `xbreaks[1]`. See “Details”.

- na.rm:

  logical value indicating whether to remove NA values before doing the
  computation of the average. This is passed to
  [`mean()`](https://rdrr.io/r/base/mean.html), which does the work of
  the present function.

## Value

A list with the following elements: the breaks (`xbreaks`, midpoints
(`xmids`) between those breaks, the count (`number`) of `x` values
between successive breaks, and the resultant average (`result`) of `f`,
classified by the `x` breaks.

## Details

By default, the sub-intervals defined by the `xbreaks` argument are open
on the left and closed on the right, to match the behaviour of
[`cut()`](https://rdrr.io/r/base/cut.html). An open interval does not
include points on the boundary, and so any `x` values that exactly match
the first `breaks` value will not be counted. To include such points in
the calculation, set `include.lowest` to TRUE.

## See also

Other bin-related functions:
[`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md),
[`binApply2D()`](https://dankelley.github.io/oce/reference/binApply2D.md),
[`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md),
[`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md),
[`binCount2D()`](https://dankelley.github.io/oce/reference/binCount2D.md),
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)

## Author

Dan Kelley

## Examples

``` r
# Plot raw temperature profile as circles, with lines indicating
# the result of averaging in 1-metre depth intervals.
library(oce)
data(ctd)
z <- ctd[["z"]]
T <- ctd[["temperature"]]
plot(T, z, cex = 0.3)
TT <- binMean1D(z, T, seq(-100, 0, 1))
lines(TT$result, TT$xmids, col = rgb(1, 0, 0, 0.9), lwd = 2)

```
