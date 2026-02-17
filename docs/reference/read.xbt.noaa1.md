# Read an xbt File in NOAA Format

This file format, described at
`https://www.aoml.noaa.gov/phod/dhos/axbt.php`, contains a header line,
followed by data lines. For example, a particular file at this site has
first three lines as follows.

    181.589 20100709 140820  -85.336  25.290 N42RF GL10 14    2010-190-15:49:18
      -0.0 27.52 -9.99
      -1.5 27.52 -9.99

where the items on the header line are (1) a year-day (ignored here),
(2) YYYYMMDD, (3) HHMMSS, (4) longitude, (5) latitude, (6) aircraft wing
number, (7) project name, (8) AXBT channel and (9) AXBT ID. The other
lines hold vertical coordinate in metres, temperature and temperature
error; -9.99 is a missing-value code. (This formatting information is
extracted from a file named `readme.axbt` that is provided with the
data.)

## Usage

``` r
read.xbt.noaa1(
  file,
  debug = getOption("oceDebug"),
  missingValue = -9.99,
  encoding = "latin1",
  processingLog
)
```

## Arguments

- file:

  character value naming a file, or a file connection, containing the
  data.

- debug:

  a flag that turns on debugging. The value indicates the depth within
  the call stack to which debugging applies.

- missingValue:

  numerical value that is to be interpreted as `NA`

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- processingLog:

  if provided, the action item to be stored in the log. This parameter
  is typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

## Value

An [xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object.

## See also

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`as.xbt()`](https://dankelley.github.io/oce/reference/as.xbt.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

## Author

Dan Kelley
