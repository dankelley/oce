# Fit a Tidal Model to a Timeseries

The fit is done in terms of sine and cosine components at the indicated
tidal frequencies (after possibly pruning to satisfy the Rayleigh
criterion, as explained in phase 2 of the procedure outlined in
“Details”), with the amplitude and phase being calculated from the
resultant coefficients on the sine and cosine terms. The scheme was
devised for hourly data; for other sampling schemes, see “Application to
non-hourly data”.

## Usage

``` r
tidem(
  t,
  x,
  constituents,
  infer = NULL,
  latitude = NULL,
  rc = 1,
  regress = lm,
  debug = getOption("oceDebug")
)
```

## Arguments

- t:

  either a vector of times or a
  [sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
  object (as created with
  [`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md)
  or
  [`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md)).
  In the first case, `x` must be provided. In the second case, though,
  any `x` that is provided will be ignored, because sealevel objects
  contain both `time` and water `elevation`, and the latter is used for
  `x`. Note that an error will be reported if any `t` values are
  non-finite.

- x:

  an optional numerical vector holding something that varies with time.
  This is ignored if `t` is a
  [sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
  object, because it is inferred automatically, using
  `t[["elevation"]]`. Note that non-finite values are permitted for `x`.

- constituents:

  an optional character vector holding the names of tidal constituents
  to which the fit is done; see “Details” and “Constituent Naming
  Convention”.

- infer:

  a list of constituents to be inferred from fitted constituents
  according to the method outlined in Section 2.3.4 of Foreman (1978).
  If `infer` is `NULL`, the default, then no such inferences are made.
  Otherwise, some constituents are computed based on other constituents,
  instead of being determined by regression at the proper frequency. If
  provided, `infer` must be a list containing four elements: `name`, a
  vector of strings naming the constituents to be inferred; `from`, a
  vector of strings naming the fitted constituents used as the sources
  for those inferences (these source constituents are added to the
  regression list, if they are not already there); `amp`, a numerical
  vector of factors to be applied to the source amplitudes; and `phase`,
  a numerical vector of angles, in degrees, to be subtracted from the
  source phases. For example, Following Foreman (1998), if any of the
  `name` items have already been computed, then the suggested inference
  is ignored, and the already-computed values are used.

      infer=list(name=c("P1","K2"),
                 from=c("K1", "S2"),
                 amp=c(0.33093, 0.27215),
                 phase=c(-7.07, -22.4)

  means that the amplitude of `P1` will be set as 0.33093 times the
  calculated amplitude of `K1`, and that the `P1` phase will be set to
  the `K1` phase, minus an offset of `-7.07` degrees. (This example is
  used in the Foreman (1978) discussion of a Fortran analysis code and
  also in Pawlowicz et al. (2002) discussion of the T_TIDE Matlab code.
  Rounded to the 0.1mm resolution of values reported in Foreman (1978)
  and Pawlowicz et al. (2002), the `tidem` results have root-mean-square
  amplitude difference to Foreman's (1978) Appendix 7.3 of 0.06mm; by
  comparison, the results in Table 1 of Pawlowicz et al. (2002) agree
  with Foreman's results to RMS difference 0.04mm.)

- latitude:

  if provided, the latitude of the observations. If not provided,
  `tidem` will try to infer this from the first argument, if it is a
  [sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
  object.

- rc:

  the value of the coefficient in the Rayleigh criterion.

- regress:

  function to be used for regression, by default
  [`lm()`](https://rdrr.io/r/stats/lm.html), but could be for example
  `rlm` from the `MASS` package.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

An object of
[tidem](https://dankelley.github.io/oce/reference/tidem-class.md),
consisting of

- const:

  constituent number, e.g. 1 for `Z0`, 1 for `SA`, etc.

- model:

  the regression model

- name:

  a vector of constituent names, in non-subscript format, e.g. "`M2`".

- frequency:

  a vector of constituent frequencies, in inverse hours.

- amplitude:

  a vector of fitted constituent amplitudes, in metres.

- phase:

  a vector of fitted constituent phase. NOTE: The definition of phase is
  likely to change as this function evolves. For now, it is phase with
  respect to the first data sample.

- p:

  a vector containing a sort of p value for each constituent. This is
  calculated as the average of the p values for the sine() and cosine()
  portions used in fitting; whether it makes any sense is an open
  question.

## Details

A summary of constituents used by `tidem()` may be found with:

    data(tidedata)
    print(tidedata$const)

A multi-stage procedure is used to establish the list of tidal
constituents to be used in the fit.

*Phase 1: initial selection.*

The initial list tidal constituents (prior to the pruning phase
described below) to be used in the analysis are specified as follows;
see “Constituent Naming Convention”.

1.  If `constituents` is not provided, then the constituent list will be
    made up of the 69 constituents designated by Foreman as "standard".
    These include astronomical frequencies and some shallow-water
    frequencies, and are as follows:
    `c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1", "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2", "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2", "ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5", "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")`.

2.  If the first item in `constituents` is the string `"standard"`, then
    a provisional list is set up as in Case 1, and then the (optional)
    rest of the elements of `constituents` are examined, in order. Each
    of these constituents is based on the name of a tidal constituent in
    the Foreman (1978) notation. (To get the list, execute
    `data(tidedata)` and then execute `cat(tideData$name)`.) Each named
    constituent is added to the existing list, if it is not already
    there. But, if the constituent is preceded by a minus sign, then it
    is removed from the list (if it is already there). Thus, for
    example, a user might specify
    `constituents=c("standard", "-M2", "ST32")` to remove the M2
    constituent and add the ST32 constituent.

3.  If the first item is not `"standard"`, then the list of constituents
    is processed as in Case 2, but without starting with the standard
    list. As an example, `constituents=c("K1", "M2")` would fit for just
    the K1 and M2 components. (It would be strange to use a minus sign
    to remove items from the list, but the function allows that.)

In each of the above cases, the list is reordered in frequency prior to
the analysis, so that the results of
[`summary,tidem-method()`](https://dankelley.github.io/oce/reference/summary-tidem-method.md)
will be in a familiar form.

*Phase 2: pruning based on Rayleigh's criterion.*

Once the initial constituent list is determined during Phase 1, `tidem`
applies the Rayleigh criterion, which holds that constituents of
frequencies \\f_1\\ and \\f_2\\ cannot be resolved unless the time
series spans a time interval of at least \\rc/(f_1-f_2)\\. The details
of the decision of which constituent to prune is fairly complicated,
involving decisions based on a comparison table. The details of this
process are provided by Foreman (1978).

*Phase 3: optionally overruling Rayleigh's criterion.*

The pruning list from phase 2 can be overruled by the user. This is done
by retaining any constituents that the user has named in the
`constituents` parameter. This action was added on 2017-12-27, to make
`tidem` behave the same way as the Foreman (1978) code, as illustrated
in his Appendices 7.2 and 7.3. (As an aside, his Appendix 7.3 has some
errors, e.g. the frequency for the 2SK5 constituent is listed there
(p58) as 0.20844743, but it is listed as 0.2084474129 in his Appendix
7.1 (p41). For this reason, the frequency comparison is relaxed to a
`tol` value of `1e-7` in a portion of the oce test suite (see
`tests/testthat/test_tidem.R` in the source).

*Comments on phases 2 and 3*

A specific example may be of help in understanding the removal of
unresolvable constituents. For example, the `data(sealevel)` dataset is
of length 6718 hours, and this is too short to resolve the full list of
constituents, with the conventional (and, really, necessary) limit of
`rc=1`. From Table 1 of Foreman (1978), this timeseries is too short to
resolve the `SA` constituent, so that `SA` will not be in the resultant.
Similarly, Table 2 of Foreman (1978) dictates the removal of `PI1`, `S1`
and `PSI1` from the list. And, finally, Table 3 of Foreman (1978)
dictates the removal of `H1`, `H2`, `T2` and `R2`, and since that
document suggests that `GAM2` be subsumed into `H1`, then if `H1` is
already being deleted, then `GAM2` will also be deleted.

## Application to non-hourly data

The framework on which `tidem()` rests on the assumption of data that
have been sampled on a 1-hour interval (see e.g. Foreman, 1977). Since
regression (as opposed to spectral analysis) is used to infer the
amplitude and phase of tidal constituents, data gaps do not pose a
serious problem. Sampling intervals under an hour are also not a
problem. However, trying to use `tidem()` on time series that are
sampled at uniform intervals that exceed 1 hour can lead to results that
are difficult to interpret. For example, some drifter data are sampled
at a 6-hour interval. This makes it impossible to fit for the S4
component (which has exactly 6 cycles per day), because the method works
by constructing sine and cosine series at tidal frequencies and using
these as the basis for regression. Each of these series will have a
constant value through the constructed time, and regression cannot
handle that (in addition to a constant-value constructed series that is
used to fit for the Z0 constituent). `tidem()` tries to handle such
problems by examining the range of the constructed sine and cosine
time-series, omitting any constituents that yield near-constant values
in either of these. Messages are issued if this problem is encountered.
This prevents failure of the regression, and the predictions of the
regression seem to represent the data reasonably well, but the inferred
constituent amplitudes are not physically reasonable. Cautious use of
`tidem()` to infer individual constituents might be warranted, but users
must be aware that the results will be difficult to interpret. The tool
is simply not designed for this use.

## Bugs

1.  This function is not fully developed yet, and both the form of the
    call and the results of the calculation may change.

2.  The reported `p` value may make no sense at all, and it might be
    removed in a future version of this function. Perhaps a significance
    level should be presented, as in the software developed by both
    Foreman and Pawlowicz.

## Constituent Naming Convention

`tidem` uses constituent names that follow the convention set by Foreman
(1978). This convention is slightly different from that used in the
T-TIDE package of Pawlowicz et al. (2002), with Foreman's `UPS1` and
`M8` becoming `UPSI` and `MS` in T-TIDE. To permit the use of either
notation, `tidem()` uses
[`tidemConstituentNameFix()`](https://dankelley.github.io/oce/reference/tidemConstituentNameFix.md)
to convert from T-TIDE names to the Foreman names, issuing warnings when
doing so.

## Agreement with `T_TIDE` results

The `tidem` amplitude and phase results, obtained with

    tidem(sealevelTuktoyaktuk, constituents=c("standard", "M10"),
        infer=list(name=c("P1", "K2"),
            from=c("K1", "S2"),
            amp=c(0.33093, 0.27215),
            phase=c(-7.07, -22.40)))

match the `T_TIDE` values listed in Table 1 of Pawlowicz et al. (2002),
after rounding amplitude and phase to 4 and 2 digits past the decimal
place, respectively, to match the format of that table.

## References

Foreman, M G., 1977 (revised 1996). Manual for Tidal Heights Analysis
and Prediction. Pacific Marine Science Report 77-10. British Columbia,
Canada: Institute of Ocean Sciences, Patricia Bay.

Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and
Prediction. Pacific Marine Science Report 78-6. British Columbia,
Canada: Institute of Ocean Sciences, Patricia Bay,

Foreman, M. G. G., Neufeld, E. T., 1991. Harmonic tidal analyses of long
time series. International Hydrographic Review, 68 (1), 95-108.

Leffler, K. E. and D. A. Jay, 2009. Enhancing tidal harmonic analysis:
Robust (hybrid) solutions. Continental Shelf Research, 29(1):78-88.

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
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# The demonstration time series from Foreman (1978),
# also used in T_TIDE (Pawlowicz, 2002).
data(sealevelTuktoyaktuk)
tide <- tidem(sealevelTuktoyaktuk)
#> Warning: tidal record too short to fit constituents: SA, SSA, MSM, MF, SIG1, RHO1, TAU1, BET1, CHI1, PI1, P1, S1, PSI1, PHI1, THE1, SO1, OQ2, 2N2, NU2, GAM2, H1, H2, MKS2, LDA2, T2, R2, K2, MSN2, SO3, MK4, SK4, 2MK6, MSK6
summary(tide)
#> tidem summary
#> -------------
#> 
#> Call:
#> tidem(t = sealevelTuktoyaktuk)
#> RMS misfit to data:  0.7808454 
#> 
#> Fitted Model:
#>         Freq Amplitude  Phase       p    
#> Z0   0.00000   1.98062   0.00 < 2e-16 ***
#> MM   0.00151   0.21213 263.34  0.0051 ** 
#> MSF  0.00282   0.15606 133.80  0.0062 ** 
#> ALP1 0.03440   0.01523 334.96  0.7368    
#> 2Q1  0.03571   0.02458  82.69  0.6516    
#> Q1   0.03722   0.01579  65.74  0.7541    
#> O1   0.03873   0.07641  74.23  0.1262    
#> NO1  0.04027   0.02903 238.14  0.3716    
#> K1   0.04178   0.13474  81.09  0.0262 *  
#> J1   0.04329   0.02530   7.32  0.5977    
#> OO1  0.04483   0.05310 235.75  0.2729    
#> UPS1 0.04634   0.02980  91.73  0.6272    
#> EPS2 0.07618   0.02115 184.60  0.6769    
#> MU2  0.07769   0.04189  83.23  0.3727    
#> N2   0.07900   0.08377  44.52  0.0723 .  
#> M2   0.08051   0.49041  77.70  0.3465    
#> L2   0.08202   0.02132  35.21  0.7301    
#> S2   0.08333   0.22024 137.48 3.1e-07 ***
#> ETA2 0.08507   0.00713 246.04  0.8902    
#> MO3  0.11924   0.01484 234.97  0.7426    
#> M3   0.12077   0.01226 261.57  0.8020    
#> MK3  0.12229   0.00492 331.60  0.9172    
#> SK3  0.12511   0.00234 237.67  0.9680    
#> MN4  0.15951   0.00917 256.47  0.8475    
#> M4   0.16102   0.01257 291.79  0.7544    
#> SN4  0.16233   0.00830 270.86  0.8659    
#> MS4  0.16384   0.00103 339.36  0.9842    
#> S4   0.16667   0.00468 299.56  0.9135    
#> 2MK5 0.20280   0.00127 310.16  0.9793    
#> 2SK5 0.20845   0.00455 104.00  0.9172    
#> 2MN6 0.24002   0.00353 271.22  0.9371    
#> M6   0.24153   0.00173 158.87  0.9681    
#> 2MS6 0.24436   0.00564 306.12  0.8938    
#> 2SM6 0.24718   0.00227 298.91  0.9555    
#> 3MK7 0.28331   0.00857 212.25  0.8508    
#> M8   0.32205   0.00304  42.38  0.9497    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> * Processing Log
#> 
#>     - 2026-04-09 19:45:23 UTC: `create 'tidem' object`
#>     - 2026-04-09 19:45:23 UTC: `tidem(t = sealevelTuktoyaktuk)`

# AIC analysis
extractAIC(tide[["model"]])
#> [1]   71.0000 -606.0823

# Fake data at M2
library(oce)
data("tidedata")
M2 <- with(tidedata$const, freq[name == "M2"])
t <- seq(0, 10 * 86400, 3600)
eta <- sin(M2 * t * 2 * pi / 3600)
sl <- as.sealevel(eta)
m <- tidem(sl)
#> Warning: tidal record too short to fit constituents: SA, SSA, MSM, MM, MSF, MF, ALP1, 2Q1, SIG1, Q1, RHO1, O1, TAU1, BET1, NO1, CHI1, PI1, P1, S1, PSI1, PHI1, THE1, J1, SO1, OO1, UPS1, OQ2, EPS2, 2N2, MU2, N2, NU2, GAM2, H1, H2, MKS2, LDA2, L2, T2, S2, R2, K2, MSN2, ETA2, MO3, SO3, MK3, SK3, MN4, SN4, MS4, MK4, S4, SK4, 2MN6, 2MS6, 2MK6, 2SM6, MSK6
summary(m)
#> tidem summary
#> -------------
#> 
#> Call:
#> tidem(t = sl)
#> RMS misfit to data:  4.034692e-15 
#> 
#> Fitted Model:
#>          Freq Amplitude Phase      p    
#> Z0   0.00e+00  1.61e-16   0.0   0.55    
#> K1   4.18e-02  5.53e-16 320.9   0.32    
#> M2   8.05e-02  1.00e+00 266.4 <2e-16 ***
#> M3   1.21e-01  2.21e-16  61.1   0.71    
#> M4   1.61e-01  5.10e-16 316.7   0.35    
#> 2MK5 2.03e-01  1.84e-16 181.6   0.81    
#> 2SK5 2.08e-01  4.12e-16  81.3   0.59    
#> M6   2.42e-01  5.66e-16 201.3   0.38    
#> 3MK7 2.83e-01  2.48e-16 252.0   0.69    
#> M8   3.22e-01  6.82e-16 136.7   0.21    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> * Processing Log
#> 
#>     - 2026-04-09 19:45:23 UTC: `create 'tidem' object`
#>     - 2026-04-09 19:45:23 UTC: `tidem(t = sl)`
```
