# Replace Parts of an xbt Object

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
# S4 method for class 'xbt'
x[[i, j, ...]] <- value
```

## Arguments

- x:

  an [xbt](https://dankelley.github.io/oce/reference/xbt-class.md)
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
`[[<-,odf-method`, `[[<-,rsk-method`, `[[<-,sealevel-method`,
`[[<-,section-method`, `[[<-,tidem-method`, `[[<-,topo-method`,
`[[<-,windrose-method`

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)
