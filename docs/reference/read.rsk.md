# Read a rsk File

Read an RBR rsk or txt file, e.g. as produced by an RBR logger,
producing an object of class `rsk`.

## Usage

``` r
read.rsk(
  file,
  from = 1,
  to,
  by = 1,
  type,
  encoding = NA,
  tz = getOption("oceTz", default = "UTC"),
  tzOffsetLocation,
  patm = FALSE,
  allTables = TRUE,
  retainDerived = TRUE,
  processingLog,
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  a connection or a character string giving the name of the RSK file to
  load. Note that `file` must be a character string, because connections
  are not used in that case, which is instead handled with database
  calls.

- from:

  indication of the first datum to read. This can a positive integer to
  indicate sequence number, the POSIX time of the first datum, or a
  character string that can be converted to a POSIX time. (For POSIX
  times, be careful about the `tz` argument.)

- to:

  an indication of the last datum to be read, in the same format as
  `from`. If `to` is missing, data will be read to the end of the file.

- by:

  an indication of the stride length to use while walking through the
  file. If this is an integer, then `by-1` samples are skipped between
  each pair of samples that is read. If this is a string representing a
  time interval, in colon-separated format (HH:MM:SS or MM:SS), then
  this interval is divided by the sampling interval, to get the stride
  length.

- type:

  optional file type, presently can be `rsk` or `txt` (for a text export
  of an RBR rsk or hex file). If this argument is not provided, an
  attempt will be made to infer the type from the file name and
  contents.

- encoding:

  ignored.

- tz:

  the timezone assumed for the time values stored in the data file.
  Unless the user has set an alternative value in the `~/.Rprofile`
  file, the default will be `"UTC"`; see the “Altering oce Defaults”
  vignette for more on the use of the `~/.Rprofile` file.

- tzOffsetLocation:

  offset, in hours, between the CTD clock and the clock in the
  controlling computer/tablet/phone (if one was used during the
  sampling). This offset is required to relate location information from
  the controller to hydrographic information from the CTD, using
  timestamps as an index (see "A note on location information" in
  “Details”). If the user supplies a value for `tzOffsetLocation`, then
  that is used. If not, an attempt is made to infer it from an item
  named `UTCdelta` that might be present within a table named `epochs`
  in the file. If no value can be inferred from either of these two
  methods, then `tzOffsetLocation` is set to zero.

- patm:

  controls the handling of atmospheric pressure, an important issue for
  RBR instruments that record absolute pressure; see “Details”.

- allTables:

  logical value, TRUE by default, indicating whether to save all the
  non-empty tables in the database (pruned of `blob` columns) in the
  `metadata` slot of the returned object. This may be useful for
  detailed analysis.

- retainDerived:

  logical value, TRUE by default, indicating whether to save derived
  quantities such as sea pressure, salinity, etc. Until version 1.8-4,
  these derived quantities were not retained.

- processingLog:

  if provided, the action item to be stored in the log. This is
  typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

- debug:

  a flag that can be set to `TRUE` to turn on debugging.

## Value

An [rsk](https://dankelley.github.io/oce/reference/rsk-class.md) object.

## Details

This can read files produced by several RBR instruments. At the moment,
five styles are understood: (1) text file produced as an export of an
RBR `hex` or `rsk` file; (2) text file with columns for temperature and
pressure (with sampling times indicated in the header); (3) text file
with four columns, in which the date the time of day are given in the
first two columns, followed by the temperature, and pressure; (4) text
file with five columns, in which depth in the water column is given
after the pressure; (5) an SQLite-based database format. The first four
options are provided mainly for historical reasons, since RBR
instruments at the date of writing commonly use the SQLite format,
though the first option is common for all instruments that produce a
`hex` file that can be read using Ruskin. Options 2-4 are mostly
obsolete, and will be removed from future versions.

**A note on location information.** It is possible to couple RBR CTD
devices with smart phones or tablets (see RBR-global, 2020), with the
location data from the latter being stored in the output `.rsk` file.
The format does not seem to be documented by RBR, but `read.rsk()`
attempts to infer location nevertheless. The procedure involves
comparing the `tstamp` (time-stamp) field from the hydrographic part of
the dataset (which is in a database table named `data`) with the
`tstamp` field in the geographical part of the dataset (in a table named
`geodata`). (The `geodata` entries are filtered to those for which the
`origin` field equals `"auto"`. This seems to align with times shown for
"LOCATION" data in RBR-provided viewing software.) The connection
between the two fields is done with
[`approx()`](https://rdrr.io/r/stats/approxfun.html), after adding
`tzOffsetLocation` (with units converted appropriately) to the time
inferred from `geodata$tstamp` field, to account for the fact that
phones and tablets may be set to local time. If the procedure succeeds,
then `longitude` and `latitude` are inserted into the `metadata` slot of
the return value, in the form of vectors with the same length as
`pressure` in the `data` slot.

**A note on conductivity.** RBR devices record conductivity in mS/cm,
and it is this value that is stored in the object returned by
`read.rsk`. This can be converted to conductivity ratio (which is what
many other instruments report) by dividing by 42.914 (see Culkin and
Smith, 1980) which will be necessary in any seawater-related function
that takes conductivity ratio as an argument (see “Examples”).

**A note on pressure.** RBR devices tend to record absolute pressure
(i.e. sea pressure plus atmospheric pressure), unlike most oceanographic
instruments that record sea pressure (or an estimate thereof). The
handling of pressure is controlled with the `patm` argument, for which
there are three possibilities. (1) If `patm` is `FALSE` (the default),
then pressure read from the data file is stored in the `data` slot of
return value, and the `metadata` item `pressureType` is set to the
string `"absolute"`. (2) If `patm` is `TRUE`, then an estimate of
atmospheric pressure is subtracted from the raw data. For data files in
the SQLite format (i.e. `*.rsk` files), this estimate will be the value
read from the file, or the “standard atmosphere” value 10.1325 dbar, if
the file lacks this information. (3) If `patm` is a numerical value (or
list of values, one for each sampling time), then \`patm\` is subtracted
from the raw data. In cases 2 and 3, an additional column named
\`pressureOriginal\` is added to the \`data\` slot to store the value
contained in the data file, and \`pressureType\` is set to a string
starting with \`"sea"\`. See
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) for
details of how this setup facilitates the conversion of
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) objects to
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects.

## References

Culkin, F., and Norman D. Smith, 1980. Determination of the
concentration of potassium chloride solution having the same electrical
conductivity, at 15 C and infinite frequency, as standard seawater of
salinity 35.0000 ppt (Chlorinity 19.37394 ppt). *IEEE Journal of Oceanic
Engineering*, **5**, pp 22-23.

RBR-global.com, 2020. "Ruskin User Guide." RBR, August 18, 2020.
Revision RBR#0006105revH.

## See also

The documentation for
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) explains
the structure of `rsk` objects, and also outlines other functions
dealing with them. Since RBR has a wide variety of instruments, `rsk`
datasets can be quite general, and it is common to coerce `rsk` objects
to other forms for specialized work, e.g.
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) can be
used to create CTD object, so that the generic plot obeys the CTD
format.

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

## Author

Dan Kelley and Clark Richards
