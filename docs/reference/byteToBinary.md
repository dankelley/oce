# Format Bytes as Binary (Defunct)

**WARNING:** The `endian` argument will soon be removed from this
function; see
[oce-defunct](https://dankelley.github.io/oce/reference/oce-deprecated.md).
This is because the actions for `endian="little"` made no sense in
practical work. The default value for `endian` was changed to `"big"` on
2017 May 6.

## Usage

``` r
byteToBinary(x, endian = "big")
```

## Arguments

- x:

  an integer to be interpreted as a byte.

- endian:

  character string indicating the endian-ness ("big" or "little").
  **WARNING:** This argument will be removed soon.

## Value

A character string representing the bit strings for the elements of `x`,
in order of significance for the `endian="big"` case. (The nibbles, or
4-bit sequences, are interchanged in the now-deprecated `"little"`
case.) See “Examples” for how this relates to the output from
[rawToBits](https://rdrr.io/r/base/rawConversion.html).

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Note comparison with rawToBits():
a <- as.raw(0x0a)
byteToBinary(a, "big") # "00001010"
#> [1] "00001010"
as.integer(rev(rawToBits(a))) # 0 0 0 0 1 0 1 0
#> [1] 0 0 0 0 1 0 1 0
```
