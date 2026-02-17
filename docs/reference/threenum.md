# Calculate Minimum, Mean, and Maximum Values

This is a simpler cousin of the standard
[`fivenum()`](https://rdrr.io/r/stats/fivenum.html) function, used in
[`summary()`](https://rdrr.io/r/base/summary.html) functions for `oce`
objects.

## Usage

``` r
threenum(x)
```

## Arguments

- x:

  a vector or matrix of numerical values.

## Value

A character vector of three values: the minimum, the mean, the maximum.

## Historical note

On Aug 5, 2019, the dimension was dropped as the fourth column, and this
function returned to the original intention (revealed by its name).
Another change is that the function now returns numerical results,
leaving the task of setting the number of digits to
[`summary()`](https://rdrr.io/r/base/summary.html).

## Author

Dan Kelley

## Examples

``` r
library(oce)
threenum(1:10)
#> [1]  1.0  5.5 10.0
```
