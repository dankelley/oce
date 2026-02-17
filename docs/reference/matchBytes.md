# Locate Byte Sequences in a Raw Vector

Find spots in a raw vector that match a given byte sequence.

## Usage

``` r
matchBytes(input, b1, ...)
```

## Arguments

- input:

  a vector of raw (byte) values.

- b1:

  a vector of bytes to match (must be of length 2 or 3 at present; for
  1-byte, use [`which()`](https://rdrr.io/r/base/which.html)).

- ...:

  additional bytes to match for (up to 2 permitted)

## Value

`matchBytes` returns a double vector of the indices of `input` that
match the start of the `bytes` sequence. (A double vector is returned
instead of an integer vector, to avoid problems with large files.)

## Author

Dan Kelley

## Examples

``` r
buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
print(buf)
#> [1] a5 11 aa a5 11 00
print(matchBytes(buf, 0xa5, 0x11))
#> [1] 1 4
```
