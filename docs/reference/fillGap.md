# Fill a Gap in an oce Object

Sequences of `NA` values, are filled by linear interpolation between the
non-`NA` values that bound the gap.

## Usage

``` r
fillGap(x, method = c("linear"), rule = 1)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- method:

  to use; see “Details”.

- rule:

  integer controlling behaviour at start and end of `x`. If `rule=1`,
  `NA` values at the ends are left in the return value. If `rule=2`,
  they are replaced with the nearest non-NA point.

## Value

A new `oce` object, with gaps removed.

## Bugs

1.  Eventually, this will be expanded to work with any `oce` object.
    But, for now, it only works for vectors that can be coerced to
    numeric.

2.  If the first or last point is `NA`, then `x` is returned unaltered.

3.  Only method `linear` is permitted now.

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Integers
x <- c(1:2, NA, NA, 5:6)
y <- fillGap(x)
print(data.frame(x, y))
#>    x y
#> 1  1 1
#> 2  2 2
#> 3 NA 3
#> 4 NA 4
#> 5  5 5
#> 6  6 6
# Floats
x <- x + 0.1
y <- fillGap(x)
print(data.frame(x, y))
#>     x   y
#> 1 1.1 1.1
#> 2 2.1 2.1
#> 3  NA 3.1
#> 4  NA 4.1
#> 5 5.1 5.1
#> 6 6.1 6.1
```
