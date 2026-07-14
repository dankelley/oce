# Plot an oce Object

This creates a [`pairs()`](https://rdrr.io/r/graphics/pairs.html) plot
of the elements in the `data` slot, if there are more than 2 elements
there, or a simple xy plot if 2 elements, or a histogram if 1 element.

## Usage

``` r
# S4 method for class 'oce'
plot(x, y, ...)
```

## Arguments

- x:

  a basic [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object, but not from any subclass that derive from this base, because
  subclasses have their own plot methods, e.g. calling
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object
  dispatches to
  [`plot,ctd-method()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md).

- y:

  Ignored; only present here because S4 object for generic `plot` need
  to have a second parameter before the `...` parameter.

- ...:

  Passed to [`hist()`](https://rdrr.io/r/graphics/hist.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), or to
  [`pairs()`](https://rdrr.io/r/graphics/pairs.html), according to
  whichever does the plotting.

## Examples

``` r
library(oce)
o <- new("oce")
o <- oceSetData(o, "x", rnorm(10))
o <- oceSetData(o, "y", rnorm(10))
o <- oceSetData(o, "z", rnorm(10))
plot(o)
```
