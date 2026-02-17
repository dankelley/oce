# Estimate Atmospheric Pressure in an rsk Object

Estimate atmospheric pressure in an
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) object.
Pressures must be in decibars for this to work. First, a subset of
pressures is created, in which the range is `sap-dp` to `sap+dp`. Here,
`sap`=10.1325 dbar is standard sealevel atmospheric pressure. Within
this window, three measures of central tendency are calculated: the
median, the mean, and a weighted mean that has weight given by
\\exp(-2\*((p-sap)/dp)^2)\\.

## Usage

``` r
rskPatm(x, dp = 0.5)
```

## Arguments

- x:

  an [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
  object.

- dp:

  Half-width of pressure window to be examined (in decibars).

## Value

A list of four estimates: `sap`, the median, the mean, and the weighted
mean.

## See also

The documentation for
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) explains
the structure of `rsk` objects, and also outlines the other functions
dealing with them.

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(rsk)
print(rskPatm(rsk))
#> [1] 10.1325 10.1325 10.1325 10.1325
```
