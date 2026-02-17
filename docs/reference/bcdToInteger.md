# Convert a BCD Value to an Integer Value

Convert a BCD Value to an Integer Value

## Usage

``` r
bcdToInteger(x, endian = c("little", "big"))
```

## Arguments

- x:

  a raw value, or vector of raw values, coded in binary-coded decimal.

- endian:

  character string indicating the endian-ness ("big" or "little"). The
  PC/intel convention is to use "little", and so most data files are in
  that format.

## Value

An integer, or list of integers.

## Author

Dan Kelley

## Examples

``` r
library(oce)
twenty.five <- bcdToInteger(as.raw(0x25))
thirty.seven <- as.integer(as.raw(0x25))
```
