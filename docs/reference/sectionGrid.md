# Grid a Section in Pressure Space

Grid a section, by interpolating to fixed pressure levels. The
`"approx"`, `"boxcar"` and `"lm"` methods are described in the
documentation for
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
which is used to do this processing.

## Usage

``` r
sectionGrid(
  section,
  p,
  method = "approx",
  trim = TRUE,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- section:

  A `section` object containing the section to be gridded.

- p:

  Optional indication of the pressure levels to which interpolation
  should be done. If this is not supplied, the pressure levels will be
  calculated based on the typical spacing in the ctd profiles stored
  within `section`. If `p="levitus"`, then pressures will be set to be
  those of the Levitus atlas, given by
  [`standardDepths()`](https://dankelley.github.io/oce/reference/standardDepths.md).
  If `p` is a single numerical value, it is taken as the number of
  subdivisions to use in a call to
  [`seq()`](https://rdrr.io/r/base/seq.html) that has range from 0 to
  the maximum pressure in `section`. Finally, if a vector numerical
  values is provided, perhaps as constructed with
  [`seq()`](https://rdrr.io/r/base/seq.html) or
  [`standardDepths`](https://dankelley.github.io/oce/reference/standardDepths.md)`(5)`
  (as in the examples), then it is used as is, after trimming any values
  that exceed the maximum pressure in the station data stored within
  `section`.

- method:

  The method to use to decimate data within the stations; see
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
  which is used for the decimation.

- trim:

  Logical value indicating whether to trim gridded pressures to the
  range of the data in `section`.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  Optional arguments to be supplied to
  [`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
  e.g. `rule` controls extrapolation beyond the observed pressure range,
  in the case where `method` equals `"approx"`.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object that contains stations in which the pressure values match
identically, and that has all flags set to `NA`.

## Details

The default `"approx"` method is best for bottle data, the `"boxcar"` is
best for ctd data, and the `"lm"` method is probably too slow to
recommend for exploratory work, in which it is common to do trials with
a variety of `"p"` values.

The stations in the returned value have flags with names that match
those of the corresponding stations in the original `section`, but the
values of these flags are all set to `NA`. This recognizes that it makes
no sense to grid flag values, but that there is merit in initializing a
flag system, for possible use in later processing steps.

## See also

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
# Gulf Stream
library(oce)
data(section)
GS <- subset(section, 113 <= stationId & stationId <= 129)
GSg <- sectionGrid(GS, p = seq(0, 5000, 100))
plot(GSg, which = "temperature")

## Show effects of various depth schemes
```
