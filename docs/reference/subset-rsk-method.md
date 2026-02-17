# Subset a rsk Object

Subset a rsk object. This function is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html), but
subsetting is only permitted by time.

## Usage

``` r
# S4 method for class 'rsk'
subset(x, subset, ...)
```

## Arguments

- x:

  an [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
  object.

- subset:

  a condition to be applied to the `data` portion of `x`. See “Details”.

- ...:

  ignored.

## Value

An [rsk](https://dankelley.github.io/oce/reference/rsk-class.md) object.

## See also

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

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
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`subset,oce-method`](https://dankelley.github.io/oce/reference/subset-oce-method.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(rsk)
plot(rsk)

plot(subset(rsk, time < mean(range(rsk[["time"]]))))

```
