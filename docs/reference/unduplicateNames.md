# Rename Duplicated Character Strings

Append numeric suffices to character strings, to avoid repeats. This is
used by various data input functions, to handle the fact that several
oceanographic data formats permit the reuse of variable names within a
given file.

## Usage

``` r
unduplicateNames(strings, style = 1)
```

## Arguments

- strings:

  Vector of character strings.

- style:

  An integer giving the style. If `style` is `1`, then e.g. a triplicate
  of `"a"` yields `"a"`, `"a1"`, and `"a2"`. If `style` is `2`, then the
  same input yields `"a_001"`, `"a_002"`, and `"a_003"`.

## Value

Vector of strings with repeats distinguished by suffix.

## See also

This is used in several functions, e.g.
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
and
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md).

## Examples

``` r
unduplicateNames(c("a", "b", "a", "c", "b"))
#> [1] "a"  "b"  "a2" "c"  "b2"
unduplicateNames(c("a", "b", "a", "c", "b"), style = 2)
#> [1] "a_001" "b_001" "a_002" "c"     "b_002"
```
