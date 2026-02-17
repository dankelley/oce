# Decode Units From Strings

This is mainly intended for internal use within the package, e.g. by
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
and so the list of string-to-unit mappings is not documented, since
developers can learn it from simple examination of the code. The focus
of `unitFromString()` is on strings that are found in oceanographic
files available to the author, *not* on all possible units.

## Usage

``` r
unitFromString(unit, scale = NULL)
```

## Arguments

- unit:

  a character value indicating the unit. These are matched according to
  rules developed to work with actual data files, and so the list is not
  by any means exhaustive.

- scale:

  a character value indicating the scale. The default value of `NULL`
  dictates that the scale is to be inferred from the unit. If a
  non-`NULL` value is supplied, it will be used, even if it makes no
  sense in relation to value of `unit`.

## Value

A [`list()`](https://rdrr.io/r/base/list.html) of two items: `unit`
which is an [`expression()`](https://rdrr.io/r/base/expression.html),
and `scale`, which is a string.

## See also

Other functions that interpret variable names and units from headers:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`unitFromStringRsk()`](https://dankelley.github.io/oce/reference/unitFromStringRsk.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)

## Examples

``` r
unitFromString("dbar") # dbar (no scale)
#> $unit
#> expression(dbar)
#> 
#> $scale
#> [1] ""
#> 
unitFromString("deg c") # modern temperature (ITS-90 scale)
#> $unit
#> expression(degree * C)
#> 
#> $scale
#> [1] "ITS-90"
#> 
```
