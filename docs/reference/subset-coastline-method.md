# Subset a coastline Object

Subsets a coastline object according to limiting values for longitude
and latitude.

## Usage

``` r
# S4 method for class 'coastline'
subset(x, subset, ...)
```

## Arguments

- x:

  a
  [coastline](https://dankelley.github.io/oce/reference/coastline-class.md)
  object.

- subset:

  An expression indicating how to subset `x`. See “Details”.

- ...:

  optional additional arguments, the only one of which is considered is
  one named `debug`, an integer that controls the level of debugging. If
  this is not supplied, `debug` is assumed to be 0, meaning no
  debugging. If it is 1, the steps of determining the bounding box are
  shown. If it is 2 or larger, then additional processing steps are
  shown, including the extraction of every polygon involved in the final
  result.

## Value

A `coastline` object.

## Details

As illustrated in the “Examples”, `subset` must be an expression that
indicates limits on both `latitude` and `longitude`. The individual
elements are provided in R notation, not mathematical notation, i.e.
`30<latitude<60` is not permitted and ought to be written
`30 < latitude & latitude < 60`. The simplest way to understand this is
to copy the example directly, and then modify the stated limits. Note
that `>` comparison is not permitted, and that `<` is converted to `<=`
in the calculation. Similarly, `&&` is converted to `&`. Spaces in the
expression are ignored. For convenience, `longitude` and and `latitude`
may be abbreviated as `lon` and `lat`, as in the “Examples”.

## See also

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineBest()`](https://dankelley.github.io/oce/reference/coastlineBest.md),
[`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
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
data(coastlineWorld)
# Subset to a box centred on Nova Scotia, Canada
if (requireNamespace("sf")) {
    cl <- subset(coastlineWorld, -80 < lon & lon <- 50 & 30 < lat & lat < 60)
    # The plot demonstrates that the trimming is as requested.
    plot(cl, clon = -65, clat = 45, span = 6000)
    rect(-80, 30, -50, 60, bg = "transparent", border = "red")
}
```
