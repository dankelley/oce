# Astronomical Calculations for tidem

Do some astronomical calculations for
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md). This
function is based directly on `t_astron` in the `T_TIDE` Matlab package
(see Pawlowicz et al. 2002), which inherits from the Fortran code
described by Foreman (1978).

## Usage

``` r
tidemAstron(t)
```

## Arguments

- t:

  Either a time in `POSIXct` format (with `"UTC"` timezone, or else odd
  behaviours may result), or an integer. In the second case, it is
  converted to a time with
  [`numberAsPOSIXct()`](https://dankelley.github.io/oce/reference/numberAsPOSIXct.md),
  using `tz="UTC"`.

## Value

A `list` containing items named `astro` and `ader` (see the `T_TIDE`
documentation).

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
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley translated this from the `t_astron` function of the `T_TIDE`
Matlab package (see Pawlowicz et al. 2002).

## Examples

``` r
tidemAstron(as.POSIXct("2008-01-22 18:50:24"))
#> $astro
#> [1] 1.44963597 0.34000639 0.83797569 0.14240012 0.08562115 0.78633081
#> 
#> $ader
#> [1] 9.661368e-01 3.660110e-02 2.737909e-03 3.094541e-04 1.470939e-04
#> [6] 1.308208e-07
#> 
```
