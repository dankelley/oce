# Subset an odf Object

This function is somewhat analogous to
[`subset.data.frame()`](https://rdrr.io/r/base/subset.html).

## Usage

``` r
# S4 method for class 'odf'
subset(x, subset, ...)
```

## Arguments

- x:

  an [odf](https://dankelley.github.io/oce/reference/odf-class.md)
  object.

- subset:

  a condition to be applied to the `data` portion of `x`. See “Details”.

- ...:

  ignored.

## Value

An [odf](https://dankelley.github.io/oce/reference/odf-class.md) object.

## Details

It seems likely that users will first convert the odf object into
another class (e.g. ctd) and use the subset method of that class; note
that some of those methods interpret the ... argument.

## See also

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)

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
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

## Author

Dan Kelley
