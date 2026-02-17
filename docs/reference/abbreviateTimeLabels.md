# Abbreviate a Vector of Times by Removing Commonalities

Abbreviate a vector of times by removing commonalities (e.g. year)

## Usage

``` r
abbreviateTimeLabels(t, ...)
```

## Arguments

- t:

  vector of times.

- ...:

  optional arguments passed to the
  [`format()`](https://rdrr.io/r/base/format.html), e.g. `format`.

## Value

None.

## See also

This is used by various functions that draw time labels on axes, e.g.
[`plot,adp-method()`](https://dankelley.github.io/oce/reference/plot-adp-method.md).

## Author

Dan Kelley, with help from Clark Richards
