# Grid Argo Float Data

Grid an Argo float, by interpolating to fixed pressure levels. The
gridding is done with
[`approx()`](https://rdrr.io/r/stats/approxfun.html). If there is
sufficient user demand, other methods may be added, by analogy to
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md).

## Usage

``` r
argoGrid(argo, p, debug = getOption("oceDebug"), ...)
```

## Arguments

- argo:

  A `argo` object to be gridded.

- p:

  Optional indication of the pressure levels to which interpolation
  should be done. If this is not supplied, the pressure levels will be
  calculated based on the existing values, using medians. If
  `p="levitus"`, then pressures will be set to be those of the Levitus
  atlas, given by
  [`standardDepths()`](https://dankelley.github.io/oce/reference/standardDepths.md),
  trimmed to the maximum pressure in `argo`. If `p` is a single
  numerical value, it is taken as the number of subdivisions to use in a
  call to [`seq()`](https://rdrr.io/r/base/seq.html) that has range from
  0 to the maximum pressure in `argo`. Finally, if a vector numerical
  values is provided, then it is used as is.

- debug:

  A flag that turns on debugging. Higher values provide deeper
  debugging.

- ...:

  Optional arguments to
  [`approx()`](https://rdrr.io/r/stats/approxfun.html), which is used to
  do the gridding.

## Value

x an [argo](https://dankelley.github.io/oce/reference/argo-class.md)
object.

## A note about flags

Data-quality flags contained within the original object are ignored by
this function, and the returned value contains no such flags. This is
because such flags represent an assessment of the original data, not of
quantities derived from those data. This function produces a warning to
this effect. The recommended practice is to use
[`handleFlags()`](https://dankelley.github.io/oce/reference/handleFlags.md)
or some other means to deal with flags before calling the present
function.

## See also

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley and Clark Richards

## Examples

``` r
library(oce)
data(argo)
g <- argoGrid(argo, p = seq(0, 100, 1))
par(mfrow = c(2, 1))
t <- g[["time"]]
z <- -g[["pressure"]][, 1]
# Set zlim because of spurious temperatures.
imagep(t, z, t(g[["temperature"]]), ylim = c(-100, 0), zlim = c(0, 20))
imagep(t, z, t(g[["salinity"]]), ylim = c(-100, 0))

```
