# Smooth an oce Object

Each data element is smoothed as a timeseries. For ADP data, this is
done along time, not distance. Time vectors, if any, are not smoothed. A
good use of `oce.smooth` is for despiking noisy data.

## Usage

``` r
oceSmooth(x, ...)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- ...:

  parameters to be supplied to
  [`smooth()`](https://rdrr.io/r/stats/smooth.html), which does the
  actual work.

## Value

An [oce](https://dankelley.github.io/oce/reference/oce-class.md) object
that has been smoothed appropriately.

## See also

The work is done with [`smooth()`](https://rdrr.io/r/stats/smooth.html),
and the `...` arguments are handed to it directly by `oce.smooth`.

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
d <- oce.smooth(ctd)
plot(d)
```
