# Subset a topo Object

This function is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html). Subsetting
can be by `time` or `distance`, but these may not be combined; use a
sequence of calls to subset by both.

## Usage

``` r
# S4 method for class 'topo'
subset(x, subset, ...)
```

## Arguments

- x:

  a [topo](https://dankelley.github.io/oce/reference/topo-class.md)
  object.

- subset:

  A condition to be applied to the `data` portion of `x`. See “Details”.

- ...:

  Ignored.

## Value

A new [topo](https://dankelley.github.io/oce/reference/topo-class.md)
object.

## See also

Other things related to topo data:
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
`[[<-,topo-method`,
[`as.topo()`](https://dankelley.github.io/oce/reference/as.topo.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`read.topo()`](https://dankelley.github.io/oce/reference/read.topo.md),
[`summary,topo-method`](https://dankelley.github.io/oce/reference/summary-topo-method.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`topoInterpolate()`](https://dankelley.github.io/oce/reference/topoInterpolate.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md)

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
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley

## Examples

``` r
# northern hemisphere
library(oce)
data(topoWorld)
plot(subset(topoWorld, latitude > 0))

```
