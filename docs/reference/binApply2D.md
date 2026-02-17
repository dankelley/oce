# Apply a Function to Matrix Data

The function `FUN` is applied to `f` in bins specified by `xbreaks` and
`ybreaks`.

## Usage

``` r
binApply2D(x, y, f, xbreaks, ybreaks, FUN, include.lowest = FALSE, ...)
```

## Arguments

- x:

  a vector of numerical values.

- y:

  a vector of numerical values.

- f:

  a vector of data to which `FUN` will be applied.

- xbreaks:

  values of `x` at the boundaries between the bins; calculated using
  [`pretty()`](https://rdrr.io/r/base/pretty.html) if not supplied.

- ybreaks:

  as `xbreaks`, but for `y`.

- FUN:

  function that is applied to the `f` values in each (x,y) bin. This
  must take two numeric vectors as input, and return a single numeric
  value.

- include.lowest:

  logical value indicating whether to include `x` values that equal
  `xbreaks[1]` and `y` values that equal `ybreaks[1]`. See “Details”.

- ...:

  optional arguments to pass to `FUN`.

## Value

A list with the following elements: `xbreaks` and `ybreaks` as used,
mid-points `xmids` and `ymids`, and `result`, a matrix containing the
result of applying `FUN()` to the `f` values in the designated bins.

## Details

The division into bins is done with
[`cut()`](https://rdrr.io/r/base/cut.html), to which `include.lowest` is
passed. By default, the `x` bins are open at the left and closed on the
right, and the `y` bins are open at the bottom and closed at the top.
However, if `include.lowest` is TRUE, then those boundary points are
included in the calculation.

## See also

Other bin-related functions:
[`binApply1D()`](https://dankelley.github.io/oce/reference/binApply1D.md),
[`binAverage()`](https://dankelley.github.io/oce/reference/binAverage.md),
[`binCount1D()`](https://dankelley.github.io/oce/reference/binCount1D.md),
[`binCount2D()`](https://dankelley.github.io/oce/reference/binCount2D.md),
[`binMean1D()`](https://dankelley.github.io/oce/reference/binMean1D.md),
[`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)

## Author

Dan Kelley
