# Fill a Gap in a Matrix

Sequences of NA values are replaced with values computed by linear
interpolation along rows and/or columns, provided that the neighbouring
values are sufficiently close, as defined by the `fillgap` parameter. If
interpolation can be done across both the row and column directions,
then the two values are averaged.

## Usage

``` r
fillGapMatrix(m, fillgap = 1, debug = getOption("oceDebug"))
```

## Arguments

- m:

  a numeric matrix.

- fillgap:

  a vector containing 1 or 2 integers, indicating the maximum width of
  gaps to be filled. If just one number is given, it is repeated to
  create the pair. The first element of the pair is the maximum gap
  height (i.e. row separation in the matrix) that can be filled, and the
  second is the maximum gap width. The default value of 1 means that
  only gaps of width or height 1 can be filled. As an exception to these
  rules, a negative value means to fill gaps regardless of size. It is
  an error to specify a `fillgap` value that is less than 1.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

fillGapMatrix returns matrix, with NA values replaced by interpolated
values as dictated by the function parameters.

## Author

Dan Kelley

## Examples

``` r
library(oce)
m <- matrix(1:20, nrow = 5)
# Example 1: interpolate past across gaps of width/height equal to 1
m[2, 3] <- NA
m[3, 3] <- NA
m[4, 2] <- NA
m
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    6   11   16
#> [2,]    2    7   NA   17
#> [3,]    3    8   NA   18
#> [4,]    4   NA   14   19
#> [5,]    5   10   15   20
fillGapMatrix(m)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    6   11   16
#> [2,]    2    7   12   17
#> [3,]    3    8   13   18
#> [4,]    4    9   14   19
#> [5,]    5   10   15   20
# Example 2: cannot interpolate across larger groups by default
m <- matrix(1:20, nrow = 5)
m[2:3, 2:3] <- NA
m
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    6   11   16
#> [2,]    2   NA   NA   17
#> [3,]    3   NA   NA   18
#> [4,]    4    9   14   19
#> [5,]    5   10   15   20
fillGapMatrix(m)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    6   11   16
#> [2,]    2   NA   NA   17
#> [3,]    3   NA   NA   18
#> [4,]    4    9   14   19
#> [5,]    5   10   15   20
# Example 3: increasing gap lets us cover gaps of size 1 or 2
fillGapMatrix(m, fillgap = 2)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    6   11   16
#> [2,]    2    7   12   17
#> [3,]    3    8   13   18
#> [4,]    4    9   14   19
#> [5,]    5   10   15   20
```
