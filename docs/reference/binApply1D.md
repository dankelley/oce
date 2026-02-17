# Apply a Function to Vector Data

The function `FUN` is applied to `f` in bins specified by `xbreaks`. The
division of data into bins is done with
[`cut()`](https://rdrr.io/r/base/cut.html).

## Usage

``` r
binApply1D(x, f, xbreaks, FUN, include.lowest = FALSE, ...)
```

## Arguments

- x:

  a vector of numerical values.

- f:

  a vector of data to which `FUN` will be applied.

- xbreaks:

  optional vector holding values of x at the boundaries between bins. If
  this is not given, it is computed by calling
  [`pretty()`](https://rdrr.io/r/base/pretty.html) with n=20 segments.

- FUN:

  function that is applied to the `f` values in each x bin. This must
  take a single numeric vector as input, and return a single numeric
  value.

- include.lowest:

  logical value indicating whether to include `x` values that equal
  `xbreaks[1]`. See “Details”.

- ...:

  optional arguments to pass to `FUN`.

## Value

A list with the following elements: `xbreaks` as used, `xmids` (the
mid-points between those breaks) and `result` (the result of applying
`FUN` to the `f` values in the designated bins).

## Details

By default, the sub-intervals defined by the `xbreaks` argument are open
on the left and closed on the right, to match the behaviour of
[`cut()`](https://rdrr.io/r/base/cut.html). An open interval does not
include points on the boundary, and so any `x` values that exactly match
the first `breaks` value will not be counted. To include such points in
the calculation, set `include.lowest` to TRUE.

## See also

Other bin-related functions:
[`binApply2D()`](https://dankelley.github.io/oce/reference/binApply2D.md),
[`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md),
[`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md),
[`binCount2D()`](https://dankelley.github.io/oce/reference/binCount2D.md),
[`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md),
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# salinity profile (black) with 1-dbar bin means (red)
data(ctd)
plotProfile(ctd, "salinity")
p <- ctd[["pressure"]]
S <- ctd[["salinity"]]
pbreaks <- seq(0, max(p), 1)
binned <- binApply1D(p, S, pbreaks, mean)
lines(binned$result, binned$xmids, lwd = 2, col = rgb(1, 0, 0, 0.9))

```
