# Nodal Modulation Calculations for Tidal Analyses

Carry out nodal modulation calculations for
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md). This
function is based directly on `t_vuf` in the `T_TIDE` Matlab package
(Pawlowicz et al., 2002), which inherits from the Fortran code described
by Foreman (1978).

## Usage

``` r
tidemVuf(t, j, latitude = NULL)
```

## Arguments

- t:

  a single time in
  [`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html) format,
  with timezone `"UTC"`.

- j:

  integer vector, giving indices of tidal constituents to use.

- latitude:

  optional numerical value containing the latitude in degrees North. If
  not provided, `u` in the return value will be a vector consisting of
  repeated 0 value, and `f` will be a vector of repeated 1 value.

## Value

A `list` containing items named `v`, `u` and `f` as described in the
`T_TIDE` documentation, as well in Pawlowicz et al. (2002) and Foreman
(1978).

## References

- Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and
  Prediction. Pacific Marine Science Report. British Columbia, Canada:
  Institute of Ocean Sciences, Patricia Bay.

- Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal
  harmonic analysis including error estimates in MATLAB using `T_TIDE`.
  Computers and Geosciences, 28, 929-937.

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley translated this from the `t_vuf` function of the `T_TIDE`
Matlab package (see Pawlowicz et al. 2002).

## Examples

``` r
# Look up values for the M2 constituent in Halifax Harbour, Canada.
library(oce)
data("tidedata")
j <- with(tidedata$const, which(name == "M2"))
tidemVuf(t = as.POSIXct("2008-01-22 18:50:24"), j = j, lat = 44.63)
#> $v
#> [1] 0.8992719
#> 
#> $u
#> [1] 0.002959181
#> 
#> $f
#> [1] 0.9689351
#> 
```
