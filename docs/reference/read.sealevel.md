# Read a sealevel File

Read a data file holding sea level data. BUG: the time vector assumes
GMT, regardless of the GMT.offset value.

## Usage

``` r
read.sealevel(
  file,
  tz = getOption("oceTz"),
  encoding = "latin1",
  processingLog,
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  either of three choices: (1) a connection, (2) a character vector of
  length 1, giving the name of the file to load (see ‘Details’ for the
  possible file formats) or (3) a character vector of length 2 giving
  the names of 2 files that are to be passed, along with `debug` (but no
  other arguments) to
  [read.sealevel.gc2026](https://dankelley.github.io/oce/reference/read.sealevel.gc2026.md).

- tz:

  time zone. The default value, `oceTz`, is set to `UTC` at setup. (If a
  time zone is present in the file header, this will supercede the value
  given here.)

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A
[sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
object.

## Details

This function starts by scanning the first line of the file, from which
it determines whether the file is in one of two known formats: type 1,
the format used at the Hawaii archive centre, and type 2, the
comma-separated-value format used by the Marine Environmental Data
Service. The file type is inferred by examination of its first line. If
that contains the string `Station_Name` the file is of type 2. If the
file is in neither of these formats, the user might wish to scan it
directly, and then to use
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md)
to create a `sealevel` object.

The Hawaii archive site at
`http://ilikai.soest.hawaii.edu/uhslc/datai.html` at one time provided a
graphical interface for downloading sealevel data in Type 1, with format
that was once described at
`http://ilikai.soest.hawaii.edu/rqds/hourly.fmt` (although that link was
observed to no longer work, on December 4, 2016). Examination of data
retrieved from what seems to be a replacement Hawaii server
(https://uhslc.soest.hawaii.edu/data/?rq) in September 2019 indicated
that the format had been changed to what is called Type 3 by
`read.sealevel`. Web searches did not uncover documentation on this
format, so the decoding scheme was developed solely through examination
of data files, which means that it might be not be correct. The MEDS
repository (`http://www.isdm-gdsi.gc.ca/isdm-gdsi/index-eng.html`)
provides Type 2 data.

## See also

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel.gc2026()`](https://dankelley.github.io/oce/reference/read.sealevel.gc2026.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)

## Author

Dan Kelley
