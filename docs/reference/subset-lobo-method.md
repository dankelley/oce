# Subset a lobo Object

Subset an lobo object, in a way that is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html).

## Usage

``` r
# S4 method for class 'lobo'
subset(x, subset, ...)
```

## Arguments

- x:

  a [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
  object.

- subset:

  a condition to be applied to the `data` portion of `x`. See “Details”.

- ...:

  ignored.

## Value

A [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
object.

## See also

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`as.lobo()`](https://dankelley.github.io/oce/reference/as.lobo.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`summary,lobo-method`](https://dankelley.github.io/oce/reference/summary-lobo-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`subset,oce-method`](https://dankelley.github.io/oce/reference/subset-oce-method.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley
