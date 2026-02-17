# Subset a met Object

This function is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html).

## Usage

``` r
# S4 method for class 'met'
subset(x, subset, ...)
```

## Arguments

- x:

  a [met](https://dankelley.github.io/oce/reference/met-class.md)
  object.

- subset:

  An expression indicating how to subset `x`.

- ...:

  ignored.

## Value

A [met](https://dankelley.github.io/oce/reference/met-class.md) object.

## See also

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`subset,oce-method`](https://dankelley.github.io/oce/reference/subset-oce-method.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(met)
# Few days surrounding Hurricane Juan
plot(subset(met, time > as.POSIXct("2003-09-27", tz = "UTC")))

```
