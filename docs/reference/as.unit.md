# Convert a String to a Unit

This converts strings to unit objects. It is designed mainly for use
within various functions in the package, not for the end user.
Therefore, the documentation does not give a full listing; for that,
developers should examine the `tests/test_that/test_units.R` file.
Developers who wish to add new entries are asked to follow the
conventions in this file, with regard to regular expressions, spaces
between tokens, etc., and also to add tests for whatever they add.

## Usage

``` r
as.unit(u, default = list(unit = expression(), scale = ""))
```

## Arguments

- u:

  a character string indicating a unit. Case is ignored, so that e.g.
  `"dbar"` and `"DBAR"` yield equal results. Many common notations are
  recognized, e.g. `kg/m^3` and `kg m-3` for density, etc.

- default:

  a default to be used for the return value, if `u` is not a recognized
  string. Setting this to NULL is a good way to discover whether a given
  value of `u` is recognized as a unit by this function, as opposed to
  something (like a conductivity ratio) that simply has no unit.

## Value

if `as.unit` recognizes `u` as unit, then it returns a list with
elements `unit`, which is an
[`expression()`](https://rdrr.io/r/base/expression.html), and `scale`,
which is a character value. That is also the case if it does not
recognize `u`, and if `default` is not specified. However, if `u` is not
recognized, and if `default` is provided by the user, then `as.unit`
returns the provided value of `default`.

## Author

Dan Kelley

## Examples

``` r
as.unit("DBAR")
#> $unit
#> expression(dbar)
#> 
#> $scale
#> [1] ""
#> 
as.unit("IPTS-68")
#> $unit
#> expression(degree * C)
#> 
#> $scale
#> [1] "IPTS-68"
#> 
as.unit("ITS-90")
#> $unit
#> expression(degree * C)
#> 
#> $scale
#> [1] "ITS-90"
#> 
as.unit("PSS-78")
#> $unit
#> expression()
#> 
#> $scale
#> [1] "PSS-78"
#> 
as.unit("UMOL/KG")
#> $unit
#> expression(mu * mol/kg)
#> 
#> $scale
#> [1] ""
#> 
```
