# Read a cm File

Read a current-meter data file, producing a
[cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

## Usage

``` r
read.cm(
  file,
  from = 1,
  to,
  by = 1,
  tz = getOption("oceTz"),
  type = c("s4"),
  longitude = NA,
  latitude = NA,
  debug = getOption("oceDebug"),
  encoding = "latin1",
  monitor = FALSE,
  processingLog
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- from:

  index number of the first measurement to be read, or the time of that
  measurement, as created with
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) (hint: use
  `tz="UTC"`).

- to:

  indication of the last measurement to read, in a format matching that
  of `from`.

- by:

  an indication of the stride length to use while walking through the
  file. If this is an integer, then `by-1` measurements are skipped
  between each pair of profiles that is read. This may not make much
  sense, if the data are not equi-spaced in time. If `by` is a string
  representing a time interval, in colon-separated format, then this
  interval is divided by the sampling interval, to get the stride
  length. *BUG:* if the data are not equi-spaced, then odd results will
  occur.

- tz:

  character string indicating time zone to be assumed in the data.

- type:

  character string indicating type of file (ignored at present).

- longitude:

  optional signed number indicating the longitude in degrees East.

- latitude:

  optional signed number indicating the latitude in degrees North.

- debug:

  a flag that turns on debugging. The value indicates the depth within
  the call stack to which debugging applies.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- monitor:

  ignored.

- processingLog:

  if provided, the action item to be stored in the log. This parameter
  is typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

## Value

An [cm](https://dankelley.github.io/oce/reference/cm-class.md) object.

The `data` slot will contain all the data in the file, with names
determined from the tokens in line 3 in that file, passed through
[`make.names()`](https://rdrr.io/r/base/make.names.html), except that
`Vnorth` is renamed `v` (after conversion from cm/s to m/s), `Veast` is
renamed `u` (after conversion from cm/s to m/s), `Cond` is renamed
`conductivity`, `T.Temp` is renamed `temperature` and `Sal` is renamed
`salinity`, and a new column named `time` (a POSIX time) is constructed
from the information in the file header, and another named `pressure` is
constructed from the column named `Depth`. At least in the single file
studied in the creation of this function, there are some columns that
are unnamed in line 3 of the header; these yield data items with names
`X`, `X.1`, etc.

## Details

There function has been tested on only a single file, and the
data-scanning algorithm was based on visual inspection of that file.
Whether it will work generally is an open question. It should be noted
that the sample file had several odd characteristics, some of which are
listed below.

- file contained two columns named `"Cond"`, which was guessed to stand
  for conductivity. Since only the first contained data, the second was
  ignored, but this may not be the case for all files.

- The unit for `"Cond"` was stated in the file to be `"mS"`, which makes
  no sense, so the unit was assumed to be mS/cm.

- The file contained a column named `"T-Temp"`, which is not something
  the author has seen in his career. It was assumed to stand for in-situ
  temperature.

- The file contained a column named `"Depth"`, which is not something an
  instrument can measure. Presumably it was calculated from pressure
  (with what atmospheric offset, though?) and so pressure was inferred
  from it using
  [`swPressure()`](https://dankelley.github.io/oce/reference/swPressure.md).

- The file contained several columns that lacked names. These were
  ignored.

- The file contained several columns that seem to be derived from the
  actual measured data, such as `"Speed"`, `"Dir"`, `"N-S Dist"`, etc.
  These are ignored.

- The file contained several columns that were basically a mystery to
  the author, e.g. `"Hx"`, `"Hy"`, `"Vref"`, etc. These were ignored.

Based on such considerations, `read.cm()` reads only the columns that
were reasonably well-understood based on the sample file. Users who need
more columns should contact the author. And a user who could produce a
document explaining the data format would be especially appreciated!

## Historical note

Prior to late July, 2016, the direction of current flow was stored in
the return value, but it is no longer stored, since it can be derived
from the `u` and `v` values.

## Changes

- On 2023-02-09 an item named `north` was added to the `metadata` slot.
  This is initialized to `"magnetic"` by `read.cm()`, but this is really
  just a guess, and users ought to consider using
  [`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md)
  to take magnetic declination into account.

## Sample of Usage

    library(oce)
    cm <- read.oce("cm_interocean_0811786.s4a.tab")
    summary(cm)
    plot(cm)

## See also

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`as.cm()`](https://dankelley.github.io/oce/reference/as.cm.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

## Author

Dan Kelley
