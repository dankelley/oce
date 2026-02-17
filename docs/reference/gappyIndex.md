# Create a Possibly Gappy Indexing Vector

This is used internally to construct indexing arrays, mainly for adv and
adp functions, in which
[`readBin()`](https://rdrr.io/r/base/readBin.html) is used to access
isolated regions within a [raw](https://rdrr.io/r/base/raw.html) vector.
The work is done in C++, for speed. Since this function is designed for
use within oce, it does not offer many safeguards on the parameters,
beyond detecting an overlapping situation that would occur if `length`
exceeded the space between `starts` elements. Also, users ought to be
aware that the behaviour of `gappyIndex()` might change at any time;
simply stated, it is not intended for direct use except by the package
developers.

## Usage

``` r
gappyIndex(starts, offset = 0L, length = 4L)
```

## Arguments

- starts:

  integer vector of one or more values.

- offset:

  integer value indicating the value to be added to each of the `starts`
  value, as the beginning of the sequence.

- length:

  integer value indicating the number of elements of that sequence.

## Details

For example, suppose data elements in a buffer named `buf` start at
bytes 1000 and 2000, and that the goal is to skip the first 4 bytes of
each of these sequences, and then to read the next 2 bytes as an
unsigned 16-bit integer. This could be accomplished as follows.

    library(oce)
    buf <- readBin("filename", "raw", n=5000, size=1)
    i <- gappyIndex(c(1000, 2000, 3000), 4, 2)
    # i is 1004,1005, 2004,2005, 3004,3005
    values <- readBin(buf[i], "integer", size=2, n=3, endian="little")

## Author

Dan Kelley
