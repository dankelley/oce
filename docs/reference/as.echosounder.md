# Coerce Data Into an echosounder Object

Coerces a dataset into a echosounder dataset.

## Usage

``` r
as.echosounder(
  time,
  depth,
  a,
  src = "",
  sourceLevel = 220,
  receiverSensitivity = -55.4,
  transmitPower = 0,
  pulseDuration = 400,
  beamwidthX = 6.5,
  beamwidthY = 6.5,
  frequency = 41800,
  correction = 0
)
```

## Arguments

- time:

  times of pings.

- depth:

  depths of samples within pings.

- a:

  matrix of echo amplitudes, as would be stored with
  [`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md).

- src:

  optional string indicating data source.

- sourceLevel:

  source level, in dB (uPa at 1m), denoted `sl` in reference 1 p15,
  where it is in units 0.1dB (uPa at 1m).

- receiverSensitivity:

  receiver sensitivity of the main element, in dB(counts/uPa), denoted
  `rs` in reference 1 p15, where it is in units of 0.1dB(counts/uPa)

- transmitPower:

  transmit power reduction factor, in dB, denoted `tpow` in reference 1
  p10, where it is in units 0.1 dB.

- pulseDuration:

  duration of transmitted pulse in us

- beamwidthX:

  x-axis -3dB one-way beamwidth in deg, denoted `bwx` in reference 1
  p16, where the unit is 0.2 deg

- beamwidthY:

  y-axis -3dB one-way beamwidth in deg, denoted `bwx` in reference 1
  p16, where the unit is 0.2 deg

- frequency:

  transducer frequency in Hz, denoted `fq` in reference 1 p16

- correction:

  user-defined calibration correction in dB, denoted `corr` in reference
  1 p14, where the unit is 0.01dB.

## Value

An
[echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
object.

## Details

Creates an echosounder file. The defaults for e.g. `transmitPower` are
taken from the `echosounder` dataset, and they are unlikely to make
sense generally. The first three parameters must be supplied, and the
dimension of `a` must align with the lengths of `time` and `depths`. The
other parameters have defaults that are unlikely to be correct for
arbitrary application, but they are not essential for basic plots, etc.

Those who use the [readHAC](https://CRAN.R-project.org/package=readHAC)
package to read echosounder data should note that it stores the `a`
matrix in a flipped and transposed format. See that package's demo code
for a function named `flip()` that transforms the matrix as required by
`as.echosounder()`. Indeed, users working with HAC data ought to study
the whole of the [readHAC](https://CRAN.R-project.org/package=readHAC)
documentation, to learn what data are stored, so that
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)
and
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
can be used as needed to flesh out the contents returned by
`as.echosounder()`.

## See also

Other things related to echosounder data:
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
`[[<-,echosounder-method`,
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`echosounder-class`](https://dankelley.github.io/oce/reference/echosounder-class.md),
[`findBottom()`](https://dankelley.github.io/oce/reference/findBottom.md),
[`plot,echosounder-method`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md),
[`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`summary,echosounder-method`](https://dankelley.github.io/oce/reference/summary-echosounder-method.md)

## Author

Dan Kelley
