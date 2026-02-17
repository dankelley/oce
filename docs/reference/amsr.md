# Sample amsr Data (Near Nova Scotia)

This is a three-day composite satellite image for July 27, 2023, trimmed
to show waters south and east of Nova Scotia, using code provide in the
“Details” section.

## Usage

``` r
data(amsr)
```

## Details

The following code was used to create this dataset.

    library(oce)
    amsr <- read.amsr(download.amsr(2023, 7, 27, destdir="~/data/amsr"))
    amsr <- subset(amsr, -71 < longitude & longitude < -60, debug=2)
    amsr <- subset(amsr,  36 < latitude  &  latitude <  45, debug=2)

## See also

Other satellite datasets provided with oce:
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md)

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

## Examples

``` r
library(oce)
data(coastlineWorld)
data(amsr)
plot(amsr, "SST")
lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])

```
