# Sample sealevel Data (Tuktoyaktuk)

This sea-level dataset is provided with in Appendix 7.2 of Foreman
(1977) and also with the `T_TIDE` package (Pawlowicz et al., 2002). It
results from measurements made in 1975 at Tuktoyaktuk, Northwest
Territories, Canada.

## Source

The data were based on the `T_TIDE` dataset, which in turn seems to be
based on Appendix 7.2 of Foreman (1977). Minor editing was on file
format, and then the `sealevelTuktoyaktuk` object was created using
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md).

## Details

The data set contains 1584 points, some of which have NA for sea-level
height.

Although Foreman's Appendix 7.2 states that times are in Mountain
standard time, the timezone is set to `UTC` in the present case, so that
the results will be similar to those he provides in his Appendix 7.3.

## Historical note

Until Jan 6, 2018, the time in this dataset had been increased by 7
hours. However, this alteration was removed on this date, to make for
simpler comparison of amplitude and phase output with the results
obtained by Foreman (1977) and Pawlowicz et al. (2002).

## References

Foreman, M. G. G., 1977. Manual for tidal heights analysis and
prediction. Pacific Marine Science Report 77-10, Institute of Ocean
Sciences, Patricia Bay, Sidney, BC, 58pp.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal
harmonic analysis including error estimates in MATLAB using `T_TIDE`.
Computers and Geosciences, 28, 929-937.

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
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)
