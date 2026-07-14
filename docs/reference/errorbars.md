# Draw Error Bars on an Existing xy Diagram

Draw Error Bars on an Existing xy Diagram

## Usage

``` r
errorbars(x, y, xe, ye, percent = FALSE, style = 0, length = 0.025, ...)
```

## Arguments

- x, y:

  coordinates of points on the existing plot.

- xe, ye:

  errors on x and y coordinates of points on the existing plot, each
  either a single number or a vector of length identical to that of the
  corresponding coordinate.

- percent:

  boolean flag indicating whether `xe` and `ye` are in terms of percent
  of the corresponding `x` and `y` values.

- style:

  indication of the style of error bar. Using `style=0` yields simple
  line segments (drawn with
  [`segments()`](https://rdrr.io/r/graphics/segments.html)) and
  `style=1` yields line segments with short perpendicular endcaps.

- length:

  length of endcaps, for `style=1` only; it is passed to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html), which is used to
  draw that style of error bars.

- ...:

  graphical parameters passed to the code that produces the error bars,
  e.g. to [`segments()`](https://rdrr.io/r/graphics/segments.html) for
  `style=0`.

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
S <- ctd[["salinity"]]
T <- ctd[["temperature"]]
plot(S, T)
errorbars(S, T, 0.05, 0.5)
```
