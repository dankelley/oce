# Show Some Values From a List, Vector or Matrix

This is similar to [`str()`](https://rdrr.io/r/utils/str.html), but it
shows data at the first and last of the vector, which can be quite
helpful in debugging.

## Usage

``` r
vectorShow(
  v,
  msg = "",
  postscript = "",
  digits = 5L,
  n = 2L,
  showNA = FALSE,
  showNewline = TRUE
)
```

## Arguments

- v:

  the item to be summarized. If this is a list of a vector of named
  items, then information is provided for each element. Otherwise,
  information is provided for the first and last `n` values.

- msg:

  optional character value indicating a message to show, introducing the
  vector. If not provided, then a message is created from `v`. If `msg`
  is a non-empty string, then that string is pasted together with a
  colon (unless `msg` already contains a colon), before pasting a
  summary of data values.

- postscript:

  optional character value indicating an optional message to append at
  the end of the return value.

- digits:

  for numerical values of `v`, this is the number of digits to use, in
  formatting the numbers with
  [`format()`](https://rdrr.io/r/base/format.html); otherwise, `digits`
  is ignored.

- n:

  number of elements to show at start and end. If `n` is negative, then
  all the elements are shown.

- showNA:

  logical value indicating whether to show the number of `NA` values.
  This is done only if the output contains ellipses, meaning that some
  values are skipped, because if all values are shown, it will be
  perfectly obvious whether there are any `NA` values.

- showNewline:

  logical value indicating whether to put a newline character at the end
  of the output string. The default, TRUE, is convenient for printing,
  but using FALSE makes more sense if the result is to be used with,
  e.g. [`mtext()`](https://rdrr.io/r/graphics/mtext.html).

## Value

A string ending in a newline character, suitable for display with
[`cat()`](https://rdrr.io/r/base/cat.html) or
[`oceDebug()`](https://dankelley.github.io/oce/reference/oceDebug.md).

## Author

Dan Kelley

## Examples

``` r
# List
limits <- list(low = 0, high = 1)
vectorShow(limits)
#> [1] "limits: low=0, high=1\n"

# Vector of named items
planktonCount <- c(phytoplankton = 100, zooplankton = 20)
vectorShow(planktonCount)
#> [1] "planktonCount: phytoplankton=100, zooplankton=20\n"

# Vector
vectorShow(pi)
#> [1] "pi: 3.1416\n"

# Matrix
vectorShow(volcano)
#> [1] "volcano[1:87, 1:61]: 100, 101, ..., 94, 94\n"

# Other arguments
knot2mps <- 0.5144444
vectorShow(knot2mps, postscript = "knots per m/s")
#> [1] "knot2mps: 0.51444 knots per m/s\n"
vectorShow("January", msg = "The first month is")
#> [1] "The first month is: January\n"
```
