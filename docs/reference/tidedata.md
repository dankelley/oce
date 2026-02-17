# Tidal Constituent Information

The `tidedata` dataset contains Tide-constituent information that is use
by [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md) to
fit tidal models. `tidedata` is a list containing

- `const`:

  a list containing vectors `name` (a string with constituent name),
  `freq` (the frequency, in cycles per hour), `kmpr` (a string naming
  the comparison constituent, blank if there is none), `ikmpr` (index of
  comparison constituent, or `0` if there is none), `df` (frequency
  difference between constituent and its comparison, used in the
  Rayleigh criterion), `d1` through `d6` (the first through sixth
  Doodson numbers), `semi`, `nsat` (number of satellite constituents),
  `ishallow`, `nshallow`, `doodsonamp`, and `doodsonspecies`.

- `sat`:

  a list containing vectors `deldood`, `phcorr`, `amprat`, `ilatfac`,
  and `iconst`.

- `shallow`:

  a list containing vectors `iconst`, `coef`, and `iname`.

Apart from the use of `d1` through `d6`, the naming and content follows
`T_TIDE` (see Pawlowicz et al. 2002), which in turn builds upon the
analysis of Foreman (1978).

## Source

The data come from the `tide3.dat` file of the `T_TIDE` package
(Pawlowicz et al., 2002), and derive from Appendices provided by Foreman
(1978). The data are scanned using `tests/tide.R` in this package, which
also performs some tests using `T_TIDE` values as a reference.

## References

Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and
Prediction. Pacific Marine Science Report. British Columbia, Canada:
Institute of Ocean Sciences, Patricia Bay.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal
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
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley
