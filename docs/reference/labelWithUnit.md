# Create Label With Unit

`labelWithUnit` creates a label with a unit, for graphical display, e.g.
by
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md).
The unit is enclosed in square brackets, although setting
`options(oceUnitBracket="(")` will cause parentheses to be used,
instead. This function is intended mainly for use within the package,
and users should not rely on its behaviour being unchangeable.

## Usage

``` r
labelWithUnit(name, unit = NULL)
```

## Arguments

- name:

  character value naming a quantity.

- unit:

  a list containing items `unit` and (optionally) `scale`, only the
  first of which, an
  [`expression()`](https://rdrr.io/r/base/expression.html), is used. If
  `unit` is not provided, then a default will be used (see “Details”).

## Value

`labelWithUnit` returns a language object, created with
[`bquote()`](https://rdrr.io/r/base/bquote.html), that that may supplied
as a text string to
[`legend()`](https://rdrr.io/r/graphics/legend.html),
[`mtext()`](https://rdrr.io/r/graphics/mtext.html),
[`text()`](https://rdrr.io/r/graphics/text.html), etc.

## Details

If `name` is in a standard list, then alterations are made as
appropriate, e.g. `"SA"` or `"Absolute Salinity"` yields an S with
subscript A; `"CT"` or `"Conservative Temperature"` yields an upper-case
Theta, `sigmaTheta` yields a sigma with subscript theta, `sigma0` yields
sigma with subscript 0 (with similar for 1 through 4), `"N2"` yields "N"
with superscript 2, and `"pressure"` yields "p". These basic
hydrographic quantities have default units that will be used if `unit`
is not supplied (see “Examples”).

In addition to the above, several chemical names are recognized, but no
unit is guessed for them, because the oceanographic community lacks
agreed-upon standards.

If `name` is not recognized, then it is simply repeated in the return
value.

## See also

Other functions that create labels:
[`resizableLabel()`](https://dankelley.github.io/oce/reference/resizableLabel.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# 1. temperature has a predefined unit, but this can be overruled
labelWithUnit("temperature")
#> T * " [" * degree * C * "]"
labelWithUnit(
    "temperature",
    list(unit = expression(m / s), scale = "erroneous")
)
#> T * " [" * (m/s) * "]"
# 2. phosphate lacks a predefined unit
labelWithUnit("phosphate")
#> PO[4]
data(section)
labelWithUnit(
    "phosphate",
    section[["station", 1]][["phosphateUnit"]]
)
#> PO[4] * " [" * (mu * mol/kg) * "]"
```
