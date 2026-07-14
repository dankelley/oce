# Tidal Current Dataset

The `tidalCurrent` dataset contains tidal velocities reported in
Foreman's (1978) report (reference 1) on his Fortran code for the
analysis of tidal currents and provided in an associated webpage
(reference 2). Here, `tidalCurrent` is data frame containing

- `time` a POSIXct time.

- `u` the eastward component of velocity in m/s.

- `v` the northward component of velocity in m/s.

## Source

The data come from the `tide8.dat` and `tide9.dat` files provided at
reference 2.

## References

1.  Foreman, M. G. G. "Manual for Tidal Currents Analysis and
    Prediction." Pacific Marine Science Report. British Columbia,
    Canada: Institute of Ocean Sciences, Patricia Bay, 1978.

2.  `https://www.dfo-mpo.gc.ca/science/documents/data-donnees/tidal-marees/tidpack.zip`

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley (reformatting data provided by Michael Foreman)

## Examples

``` r
library(oce)
data(tidalCurrent)
par(mfrow = c(2, 1))
oce.plot.ts(tidalCurrent$time, tidalCurrent$u, ylab = "u [m/s]")
abline(h = 0, col = 2)
oce.plot.ts(tidalCurrent$time, tidalCurrent$v, ylab = "v [m/s]")
abline(h = 0, col = 2)

```
