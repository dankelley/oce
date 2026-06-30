# Create tidem Object From Fitted Harmonic Data

This function takes a set of tidal constituent amplitudes and phases,
and constructs a return value of similar form to that returned by
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md). Its
purpose is to enable predictions based on published constituent
amplitudes and phases. Since `as.tidem()` does not account for a
reference height, it is the user's responsible to account for this after
a prediction is made using
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md).

## Usage

``` r
as.tidem(
  tRef,
  latitude,
  name,
  amplitude,
  phase,
  frequency,
  speed,
  debug = getOption("oceDebug")
)
```

## Arguments

- tRef:

  a POSIXt value indicating the mean time of the observations used to
  develop the harmonic model. This is rounded to the nearest hour in
  `as.tidem()`, to match the behaviour of
  [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md).

- latitude:

  numerical value indicating the latitude of the observations that were
  used to create the harmonic model. This is needed for nodal-correction
  procedures carried out by
  [`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md).

- name:

  character vector holding names of constituents.

- amplitude, phase:

  numeric vectors of constituent amplitudes and phases. These must be of
  the same length as `name`.

- frequency, speed:

  optional numeric vectors giving the frequencies of the constituents
  (in cycles per hour) or the analogous speeds (in degrees per hour).
  Only one of these may be given, and a conversion is done from the
  latter to the former, if required. If the frequencies are thus
  specified, then these are used instead of the frequencies that oce
  normally used, as defined in `data(tideconst)`. A warning will be
  issued if the absolute value of the relative frequency mismatch for
  any given component exceeds 1e-6, and this will occur for any NOAA
  tables containing the SA component, for which this relative mismatch
  is approximately 4e-5 (see reference 5).

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
[tidem](https://dankelley.github.io/oce/reference/tidem-class.md), with
only minimal contents.

## Details

All the constituent names used by
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md) are
permitted here, *except* for `"Z0"` (see “Description” regarding
reference height). To get a list of constituent names, please consult
Foreman (1978), or type the following in an R console:


    data(tidedata)
    data.frame(name=tidedata$const$name, freq=tidedata$const$freq)

In addition to the above, `as.tidem()` can handle NOAA names of
constituents. For the most part, these match oce names, but there are 4
exceptions: NOAA names `"LAM2"`, `"M1"`, `"RHO"`, and `"2MK3"` are
converted to oce names `"LDA2"`, `"NO1"`, `"RHO1"`, and `"MO3"`. The
name mapping was inferred by matching frequencies; for these
constituents, the fractional mismatch in frequencies was under 4e-8; see
Reference 5 for more details. A message is printed if these name
conversions are required in the particular use of `as.tidem()`.

Apart from the standard oce names and this set of NOAA synonyms, any
other constituent name is reported in a warning message.

## Known issues

There are two known differences between
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md) and the
Matlab `T_TIDE` package, as listed in references 3 and 4.

## References

1.  Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and
    Prediction. Pacific Marine Science Report. British Columbia, Canada:
    Institute of Ocean Sciences, Patricia Bay.

2.  Wikipedia, "Theory of Tides."
    https://en.wikipedia.org/wiki/Theory_of_tides Downloaded Aug 17,
    2019.

3.  Github issue 1653 "tidem() and t_tide do not produce identical
    results" (https://github.com/dankelley/oce/issues/1653)

4.  Github issue 1654 "predict(tidem()) uses all constituents, unlike
    T_TIDE" (https://github.com/dankelley/oce/issues/1654)

5.  Github issue 2143 "mismatch in oce/NOAA frequency of SA tidal
    constituent" (https://github.com/dankelley/oce/issues/2143)

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Examples

``` r

# Example 1: show agreement with tidem()
data(sealevelTuktoyaktuk)
# 'm0' is model fitted by tidem()
m0 <- tidem(sealevelTuktoyaktuk)
#> Warning: tidal record too short to fit constituents: SA, SSA, MSM, MF, SIG1, RHO1, TAU1, BET1, CHI1, PI1, P1, S1, PSI1, PHI1, THE1, SO1, OQ2, 2N2, NU2, GAM2, H1, H2, MKS2, LDA2, T2, R2, K2, MSN2, SO3, MK4, SK4, 2MK6, MSK6
p0 <- predict(m0, sealevelTuktoyaktuk[["time"]])
m1 <- as.tidem(
    mean(sealevelTuktoyaktuk[["time"]]), sealevelTuktoyaktuk[["latitude"]],
    m0[["name"]], m0[["amplitude"]], m0[["phase"]]
)
# Test agreement with tidem() result, by comparing predicted sealevels.
p1 <- predict(m1, sealevelTuktoyaktuk[["time"]])
stopifnot(max(abs(p1 - p0), na.rm = TRUE) < 1e-10)

# Example 2: See the effect of dropping weak constituents
m0[["name"]][which(m0[["amplitude"]] > 0.05)]
#> [1] "Z0"  "MM"  "MSF" "O1"  "K1"  "OO1" "N2"  "M2"  "S2" 
h <- "
name  amplitude      phase
  Z0 1.98061875   0.000000
  MM 0.21213065 263.344739
 MSF 0.15605629 133.795004
  O1 0.07641438  74.233130
  K1 0.13473817  81.093134
 OO1 0.05309911 235.749693
  N2 0.08377108  44.521462
  M2 0.49041340  77.703594
  S2 0.22023705 137.475767"
coef <- read.table(text = h, header = TRUE)
m2 <- as.tidem(
    mean(sealevelTuktoyaktuk[["time"]]),
    sealevelTuktoyaktuk[["latitude"]],
    coef$name, coef$amplitude, coef$phase
)
p2 <- predict(m2, sealevelTuktoyaktuk[["time"]])
par(mfrow = c(3, 1))
oce.plot.ts(sealevelTuktoyaktuk[["time"]], p0)
ylim <- par("usr")[3:4] # to match scales in other panels
oce.plot.ts(sealevelTuktoyaktuk[["time"]], p1, ylim = ylim)
oce.plot.ts(sealevelTuktoyaktuk[["time"]], p2, ylim = ylim)

```
