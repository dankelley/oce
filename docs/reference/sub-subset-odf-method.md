# Replace Parts of an odf Object

The `[[<-` method works for all
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.
The purpose, as with the related extraction method, `[[`, is to insulate
users from the internal details of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects,
by looking for items within the various storage slots of the object.
Items not actually stored can also be replaced, including units and
data-quality flags.

## Usage

``` r
# S4 method for class 'odf'
x[[i, j, ...]] <- value
```

## Arguments

- x:

  an [odf](https://dankelley.github.io/oce/reference/odf-class.md)
  object.

- i:

  character value naming the item to replace.

- j:

  optional additional information on the `i` item.

- ...:

  optional additional information (ignored).

- value:

  The value to be placed into `x`, somewhere.

## Details

As with `[[` method, the procedure works in steps.

First, the `metadata` slot of `x` is checked to see whether it contains
something named with `i`. If so, then the named item is replaced with
`value`.

Otherwise, if the string value of `i` ends in `Unit`, then the
characters preceding that are taken as the name of a variable, and the
`metadata` slot of `x` is updated to store that unit, e.g.

    x[["temperatureUnits"]] <- list(unit=expression(degree*F),scale="")

Similarly, if `i` ends in `Flag`, then quality-control flags are set up
as defined by `result`, e.g.

    o[["temperatureFlags"]] <- c(2,4,2,2)

Otherwise, [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used for
a partial-string match with the names of the items that are in the
`data` slot of `x`. The first item found (if any) is then updated to
hold the value `result`.

If none of these conditions is met, a warning is issued.

## See also

Other functions that replace parts of oce objects: `[[<-,adp-method`,
`[[<-,amsr-method`, `[[<-,argo-method`, `[[<-,bremen-method`,
`[[<-,cm-method`, `[[<-,coastline-method`, `[[<-,ctd-method`,
`[[<-,echosounder-method`, `[[<-,g1sst-method`, `[[<-,gps-method`,
`[[<-,ladp-method`, `[[<-,landsat-method`, `[[<-,lisst-method`,
`[[<-,lobo-method`, `[[<-,met-method`, `[[<-,oce-method`,
`[[<-,rsk-method`, `[[<-,sealevel-method`, `[[<-,section-method`,
`[[<-,tidem-method`, `[[<-,topo-method`, `[[<-,windrose-method`,
`[[<-,xbt-method`

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)
