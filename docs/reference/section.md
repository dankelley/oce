# Sample section Data

This is from a 1993 occupation of WOCE line A03 that ran westward across
the Atlantic at approximately 36 N, ending with a turn to the northwest
in the Gulf Stream region.

## Usage

``` r
data(section)
```

## Source

The following code was used to download the datafile and create the
`section` object.

    download.file("https://cchdo.ucsd.edu/data/7872/a03_hy1.csv",
        "a03_hy1.csv")
    section <- read.section("a03_hy1.csv",
        sectionId = "a03", institute = "SIO",
        ship = "R/V Professor Multanovskiy",
        scientist = "Vladimir Tereschenkov")

## Details

The sampling times in this dataset are not fully sequential. For
example, Station 41 is reported to be at `1993-10-03T00:06:00`, which,
going by station numbers, suggests an error in the reported day. Station
45 seems to have a similar problem. However, the goal here is to
represent the data as archived, so no changes are made to the times.

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
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Examples

``` r
library(oce)
# Gulf Stream
data(section)
GS <- subset(section, 113 <= stationId & stationId <= 129)
GSg <- sectionGrid(GS, p = seq(0, 5000, 100))
plot(GSg, span = 1500) # increase span to show more coastline

```
