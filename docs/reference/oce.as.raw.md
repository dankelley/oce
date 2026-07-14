# Version of as.raw() That Clips Data

A version of as.raw() that clips data to prevent warnings

## Usage

``` r
oce.as.raw(x)
```

## Arguments

- x:

  values to be converted to raw

## Value

Raw values corresponding to `x`.

## Details

Negative values are clipped to 0, while values above 255 are clipped to
255; the result is passed to
[`as.raw()`](https://rdrr.io/r/base/raw.html) and returned.

## Author

Dan Kelley

## Examples

``` r
x <- c(-0.1, 0, 1, 255, 255.1)
data.frame(x, oce.as.raw(x))
#>       x oce.as.raw.x.
#> 1  -0.1            00
#> 2   0.0            00
#> 3   1.0            01
#> 4 255.0            ff
#> 5 255.1            ff
```
