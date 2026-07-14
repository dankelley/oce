# Subset an argo Object

Subset an argo object, either by selecting just the "adjusted" data or
by subsetting by pressure or other variables.

## Usage

``` r
# S4 method for class 'argo'
subset(x, subset, ...)
```

## Arguments

- x:

  an [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  object.

- subset:

  An expression indicating how to subset `x`.

- ...:

  optional arguments, of which only the first is examined. The only
  possibility is `within`, a polygon enclosing data to be retained. This
  must be either a list or data frame, containing items named either `x`
  and `y` or `longitude` and `latitude`; see Example 4. If `within` is
  given, then `subset` is ignored.

## Value

An [argo](https://dankelley.github.io/oce/reference/argo-class.md)
object.

## Details

If `subset` is the string `"adjusted"`, then `subset` replaces the
station variables with their adjusted counterparts. In the argo
notation, e.g. `PSAL` is replaced with `PSAL_ADJUSTED`; in the present
notation, this means that `salinity` in the `data` slot is replaced with
`salinityAdjusted`, and the latter is deleted. Similar replacements are
also done with the flags stored in the `metadata` slot.

If `subset` is an expression, then the action is somewhat similar to
other `subset` functions, but with the restriction that only one
independent variable may be used in in any call to the function, so that
repeated calls will be necessary to subset based on more than one
independent variable. Subsetting may be done by anything stored in the
data, e.g. `time`, `latitude`, `longitude`, `profile`, `dataMode`, or
`pressure` or by `profile` (a made-up variable), `id` (from the
`metadata` slot) or `ID` (a synonym for `id`). Note that subsetting by
`pressure` preserves matrix shape, by setting discarded values to `NA`,
as opposed to dropping data (as is the case with `time`, for example).

## Sample of Usage


    # Example 2: restrict attention to delayed-mode profiles.
    par(mfrow=c(1, 1))
    plot(subset(argo, dataMode == "D"))

    # Example 3: contrast adjusted and unadjusted data
    par(mfrow=c(1, 2))
    plotTS(argo)
    plotTS(subset(argo, "adjusted"))

    # Example 2. Subset by a polygon determined with locator()
    par(mfrow=c(1, 2))
    plot(argo, which="map")
    # Can get a boundary with e.g. locator(4)
    boundary <- list(x=c(-65, -40, -40, -65), y=c(65, 65, 45, 45))
    argoSubset <- subset(argo, within=boundary)
    plot(argoSubset, which="map")

## See also

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
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
data(argo)

# Example 1: subset by time, longitude, and pressure
par(mfrow = c(2, 2))
plot(argo)
plot(subset(argo, time > mean(time)))
plot(subset(argo, longitude > mean(longitude)))
plot(subset(argoGrid(argo), pressure > 500 & pressure < 1000), which = 5)

```
