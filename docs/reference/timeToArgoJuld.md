# Convert Time to Argo Julian Day (juld)

This converts a POSIXct time into an Argo julian day, with the
convention that juld=0 at 1950-01-01.

## Usage

``` r
timeToArgoJuld(t)
```

## Arguments

- t:

  A POSIXct time or a string that can be converted to a POSIXct time

## Author

Jaimie Harbin and Dan Kelley

## Examples

``` r
timeToArgoJuld("2020-07-01")
#> [1] 25749
```
