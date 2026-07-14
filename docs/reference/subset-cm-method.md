# Subset a cm Object

This function is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html).

## Usage

``` r
# S4 method for class 'cm'
subset(x, subset, ...)
```

## Arguments

- x:

  a [cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

- subset:

  a condition to be applied to the `data` portion of `x`. See “Details”.

- ...:

  ignored.

## Value

A new `cm` object.

## See also

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
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

## Examples

``` r
library(oce)
data(cm)
plot(cm)

plot(subset(cm, time < mean(range(cm[["time"]]))))

```
