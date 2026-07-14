# Partial Matching of Strings or Numbers

An extended version of [`pmatch()`](https://rdrr.io/r/base/pmatch.html)
that allows `x` to be numeric or string-based. As with
[`pmatch()`](https://rdrr.io/r/base/pmatch.html), partial string matches
are handled. This is a wrapper that is useful mainly for `which`
arguments to plotting functions.

## Usage

``` r
ocePmatch(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
```

## Arguments

- x:

  a code, or vector of codes. This may be numeric, in which case it is
  simply returned without further analysis of the other arguments, or it
  may be string-based, in which case
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used to find
  numeric matches.

- table:

  a list that maps strings to numbers;
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used on
  `names(table)`. If the name contains characters that are normally not
  permitted in a variable name, use quotes, e.g.
  `list(salinity=1, temperature=2, "salinity+temperature"=3)`.

- nomatch:

  value to be returned for cases of no match (passed to
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html).

- duplicates.ok:

  code for the handling of duplicates (passed to
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html)).

## Value

A number, or vector of numbers, corresponding to the matches.
Non-matches are indicated with `NA` values, or whatever value is given
by the `NA` argument.

## See also

Since [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used for the
actual matching, its documentation should be consulted.

## Author

Dan Kelley

## Examples

``` r
library(oce)
oce.pmatch(c("s", "at", "te"), list(salinity = 1, temperature = 3.1))
#> [1] 1.0  NA 3.1
```
