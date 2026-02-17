# Subset an amsr Object

Return a subset of a
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) object.

## Usage

``` r
# S4 method for class 'amsr'
subset(x, subset, ...)
```

## Arguments

- x:

  an [amsr](https://dankelley.github.io/oce/reference/amsr-class.md)
  object.

- subset:

  an expression indicating how to subset `x`.

- ...:

  ignored.

## Value

An [amsr](https://dankelley.github.io/oce/reference/amsr-class.md)
object.

## Details

This function is used to subset data within an
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) object
by `longitude` or by `latitude`. These two methods cannot be combined in
a single call, so two calls are required, as shown in the Example.

## See also

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
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
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(amsr) # see ?amsr for how to read and composite such objects
sub <- subset(amsr, -75 < longitude & longitude < -45)
sub <- subset(sub, 40 < latitude & latitude < 50)
plot(sub)
data(coastlineWorld)
lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])

```
