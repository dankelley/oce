# Convert From Snake-Case to Camel-Case Notation

`snakeToCamel` converts "snake-case" characters such as `"NOVA_SCOTIA"`
to "camel-case" values, such as `"NovaScotia"`. It was written for use
by
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
but it also may prove helpful in other contexts.

## Usage

``` r
snakeToCamel(s, specialCases = NULL)
```

## Arguments

- s:

  A vector of character values.

- specialCases:

  A vector of character values that tell which special-cases to apply,
  or `NULL` (the default) to turn off special cases. The only permitted
  special case at the moment is `"QC"` (see “Details”) but the idea of
  this argument is that other cases can be added later, if needed.

## Value

A vector of character values

## Details

The basic procedure is to chop the string up into substrings separated
by the underline character, then to upper-case the first letter of all
substrings except the first, and then to paste the substrings together.

However, there are exceptions. First, any upper-case string that
contains no underlines is converted to lower case, but any mixed-case
string with no underlines is returned as-is (see the second example).
Second, if the `specialCases` argument contains `"QC"`, then the `QC` is
passed through directly (since it is an acronym) and if the first letter
of remaining text is upper-cased (contrast see the four examples).

## Author

Dan Kelley

## Examples

``` r
library(oce)
snakeToCamel("PARAMETER_DATA_MODE") # "parameterDataMode"
#> [1] "parameterDataMode"
snakeToCamel("PARAMETER") # "parameter"
#> [1] "parameter"
snakeToCamel("HISTORY_QCTEST") # "historyQctest"
#> [1] "historyQctest"
snakeToCamel("HISTORY_QCTEST", "QC") # "historyQCTest"
#> [1] "historyQCTest"
snakeToCamel("PROFILE_DOXY_QC") # "profileDoxyQc"
#> [1] "profileDoxyQc"
snakeToCamel("PROFILE_DOXY_QC", "QC") # "profileDoxyQC"
#> [1] "profileDoxyQC"
```
