# Sample met Data

This is sample
[met](https://dankelley.github.io/oce/reference/met-class.md) object
containing data for Halifax, Nova Scotia, during September of 2003 (the
period during which Hurricane Juan struck the city).

## Source

Environment Canada website on October 19, 2019.

## Details

The data file was downloaded

    metFile <- download.met(id=6358, year=2003, month=9, destdir=".", type="xml")

Note that using
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md)
avoids having to navigate the the awkward Environment Canada website,
but it imposes the burden of having to know the station ID number. With
the data in-hand, the object was then created (and its timezone
adjusted) with

    met <- read.met(metFile)
    met <- oceSetData(met, "time", met[["time"]]+4*3600,
                     note="add 4h to local time to get UTC time")

*Historical note.* The `data(met)` object was changed on October 19,
2019, based on the data provided by Environment Canada at that time. The
previous version of `data(met)`, created in 2017, had been based on a
data format that Environment Canada no longer provided in 2019. See the
notes on the `type` argument of
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
for more on this shift in the Environment Canada data format.

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)
