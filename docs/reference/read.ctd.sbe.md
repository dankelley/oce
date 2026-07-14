# Read a ctd File in Seabird Format

Read a ctd File in Seabird Format

## Usage

``` r
read.ctd.sbe(
  file,
  columns = NULL,
  station = NULL,
  missingValue,
  deploymentType = "unknown",
  btl = FALSE,
  monitor = FALSE,
  encoding = "latin1",
  rename = TRUE,
  requireSalinity = TRUE,
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
```

## Arguments

- file:

  either a connection or a character value naming a file. For
  `read.ctd.sbe()` and
  [`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
  this may be a wildcard (e.g. `"*.cnv"` or `"*.csv"`) in which case the
  return value is a vector containing CTD objects created by reading the
  files from [`list.files()`](https://rdrr.io/r/base/list.files.html)
  with `pattern` set to the specified wildcard pattern.

- columns:

  an optional [list](https://rdrr.io/r/base/list.html) that can be used
  to convert unrecognized data names to resultant variable names. This
  is used only by `read.ctd.sbe()` and
  [`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md).
  For example, if a data file named salinity as `"SAL"`, then using

      d <- read.ctd(f, columns=list(
          salinity=list(name="SAL",
                        unit=list(unit=expression(),
                        scale="PSS-78"))))

  would assign the `"SAL"` column to the `salinity` entry in the data
  slot of the CTD object returned by the `read.*` function.

- station:

  optional character string containing an identifying name or number for
  the station. This can be useful if the routine cannot determine the
  name automatically, or if another name is preferred.

- missingValue:

  optional missing-value flag; data matching this value will be set to
  `NA` upon reading. If this is provided, then it overrules any
  missing-value flag found in the data. For Seabird (`.cnv`) files,
  there is usually no need to set `missingValue`, because it can be
  inferred from the header (typically as -9.990e-29). Set
  `missingValue=NULL` to turn off missing-value detection, even in
  `.cnv` files that contain missing-value codes in their headers. If
  `missingValue` is not specified, then an attempt is made to infer such
  a value from the data, by testing whether salinity and/or temperature
  has a minimum that is under -8 in value; this should catch common
  values in files, without false positives. A warning will be issued in
  this case, and a note inserted in the processing log of the return
  value.

- deploymentType:

  character string indicating the type of deployment. Use `"unknown"` if
  this is not known, `"profile"` for a profile (in which the data were
  acquired during a downcast, while the device was lowered into the
  water column, perhaps also including an upcast; `"moored"` if the
  device is installed on a fixed mooring, `"thermosalinograph"` (or
  `"tsg"`) if the device is mounted on a moving vessel, to record
  near-surface properties, or `"towyo"` if the device is repeatedly
  lowered and raised.

- btl:

  a logical value, with `TRUE` indicating that this is a `.BTL` file and
  `FALSE` (the default) indicating a `.CNV` file. Note that if `btl` is
  `TRUE`, the data column names are taken directly from the file
  (without e.g. translating to `"Sal00"` to `"salinity"`. Also, the
  "avg" and "sdev" columns are blended together, with all the latter
  named as in the file, but with `"_sdev"` appended.

- monitor:

  boolean, set to `TRUE` to provide an indication of progress. This is
  useful if `filename` is a wildcard.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- rename:

  optional logical value indicating whether to rename variables from the
  values in the file to the oce convention, using
  [`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md)
  for the translation. This is done by default, but setting
  `rename=FALSE` can be helpful if there is a wish to control the
  renaming, either using a built-in dictionary or using a dictionary set
  up by the user. See Examples 1 and 2, the latter dealing with the case
  of using
  [`oceRename()`](https://dankelley.github.io/oce/reference/oceRename.md)
  to rename the variables after the file has been read.

- requireSalinity:

  logical value indicating what to do if the dataset lacks both salinity
  and conductivity. If `requireSalinity` is TRUE (the default) then an
  error results in this situation; otherwise, a warning is issued.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed.

- processingLog:

  if provided, the action item to be stored in the log. This is
  typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

- ...:

  additional arguments, passed to called routines.

## Value

This function returns a
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.

## Details

This function reads files stored in Seabird `.cnv` format. Note that
these files can contain multiple sensors for a given field. For example,
the file might contain a column named `t090C` for one temperature sensor
and `t190C` for a second. The first will be denoted `temperature` in the
`data` slot of the return value, and the second will be denoted
`temperature1`. This means that the first sensor will be used in any
future processing that accesses `temperature`. This is for convenience
of processing, and it does not pose a limitation, because the data from
the second sensor are also available as e.g. `x[["temperature1"]]`,
where `x` is the name of the returned value. For the details of the
mapping from `.cnv` names to `ctd` names, see
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md).

The names of the elements in the `data` slot of the returned value
depend on the file type, as signalled by the `btl` argument. For the
default case of `.cnv` files, the original data names as stored in
`file` are stored within the `metadata` slot as `dataNamesOriginal`, and
are displayed with `summary` alongside the numerical summary; see the
Appendix VI of reference 2 for the meanings of these names (in the
"Short Name" column of the table spanning pages 161 through 172).
However, for the case of `.btl` files, the column names are as described
in the documentation entry for the `btl` argument.

## A note on freshwater files

As of version 1.8-4 of the package, `read.ctd.sbe()` can handle files
that lack both salinity data and conductivity data. The existence of
such files is possible for sampling done in lakes. In prior versions,
the function reported an error, but as of 1.8-4, it reports only a
warning. The recommended practice in such a case is to do e.g.

    d <- read.ctd.sbe("file.cnv")
    d <- ctdAddData(d, "salinity", rep(0.0, length(d[["pressure"]]))

to add a salinity column. That will permit e.g. density calculations,
although the accuracy of such calculations is questionable, since the
equations of state are meant to apply to salt water.

## A note on hand-entered headers

CNV files may have a section that contains human-entered information.
This is detected by `read.ctd.sbe()` as lines that begin with two
asterisks. Decoding this information can be tricky, because humans have
many ways of writing things.

For example, consider the `date` item in the `metadata` slot of the
returned value. `read.ctd.sbe()` infers this value in one of two ways.
First, if there is a header line staring with

    * NMEA UTC (Time) =

then that value is decoded and used for `date`. This header line,
preceded by a single asterisk, is not human-entered, and so there is
reason to hope for a uniform format that can be handled by
`read.ctd.sbe()`. However, if there is no NMEA header line, then
`read.ctd.sbe()` will look for a line starting with

    ** Date:

which was human-entered. This is the second choice, because humans write
dates in a bewildering variety of ways, and
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), which
`read.ctd.sbe` uses to parse the date, cannot handle them all. If there
is a problem, `read.ctd.sbe()` issues a warning and stores NA in `date`.

A similar error-detection procedure is used for human-entered location
data, which appear in lines starting with either

    ** Longitude:

or

    ** Latitude:

which often take forms that `read.ctd.sbe()` cannot parse.

It is important to note that, even if no warnings are issued, there is a
reasonably high chance that human-entered data will be scanned
incorrectly. (Did the operator remember to indicate the hemisphere? Does
123.456 indicate decimal degrees, or 123 degrees plus 45.6 minutes? Is
hemisphere indicated by sign or by letter, and, if the latter, where
does it appear?)

In deep-sea work, a ship might steam for 6 hours between CTD stations,
so the ship-time cost of each CTD file can be several thousand dollars.
Surely it is not unreasonable for an analyst to take a minute to glance
at the CNV file, to ascertain whether `read.ctd.sbe()` inferred correct
values.

[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)
is helpful for correcting problems with individual files, but if many
files are systematically problematic, say for a whole cruise or perhaps
even for a whole institution, then it might sense to set up a wrapper
function to correct deficiencies in the CNV files. As an example, the
following handles dates specified in a particular nonstandard way.

    read.ctd.sbe.wrapper <- function(cnv)
    {
        lines <- readLines(cnv)
        # Change month-day-year to year-month-day, so as.POSIXct() can parse it.
        lines <- gsub("^\\*\\* Date: (.*)-(.*)-(.*)", "** Date: \\3-\\1-\\2", lines)
        read.ctd.sbe(textConnection(lines))
    }

## A note on sampling times

Until November of 2018, there was a possibility for great confusion in
the storage of the time entries within the `data` slot, because
`read.ctd.sbe` renamed each of the ten variants of time (see reference 2
for a list) as `"time"` in the `data` slot of the returned value. For
CTD profiles, this was perhaps not a great problem, but it could lead to
significant confusion for moored data. Therefore, a change to
`read.ctd.sbe` was made, so that it would Seabird times, using the
`start_time` entry in the CNV file header (which is stored as
`startTime` in the object `metadata` slot), along with specific time
columns as follows (and as documented, with uneven clarity, in the SBE
Seasoft data processing manual, revision 7.26.8, Appendix VI):

|  |  |
|----|----|
| **Item** | **Meaning** |
| `timeS` | seconds elapsed since `start_time` |
| `timeM` | minutes elapsed since `start_time` |
| `timeH` | hours elapsed since `start_time` |
| `timeJ` | Julian days since the start of the year of the first observation |
| `timeN` | NMEA-based time, in seconds past Jan 1, 1970 |
| `timeQ` | NMEA-based time, in seconds past Jan 1, 2000 |
| `timeK` | NMEA-based time, in seconds past Jan 1, 2000 |
| `timeJV2` | as `timeJ` |
| `timeSCP` | as `timeJ` |
| `timeY` | computer time, in seconds past Jan 1, 1970 |

NOTE: not all of these times have been tested properly, and so users are
asked to report incorrect times, so that `read.ctd.sbe` can be improved.

## A note on scales

The user might encounter data files with a variety of scales for
temperature and salinity. Oce keeps track of these scales in the units
it sets up for the stored variables. For example, if `A` is a CTD
object, then `A[["temperatureUnit"]]$scale` is a character string that
will indicate the scale. Modern-day data will have `"ITS-90"` for that
scale, and old data may have `"IPTS-68"`. The point of saving the scale
in this way is so that the various formulas that deal with water
properties can account for the scale, e.g. converting from numerical
values saved on the `"IPTS-68"` scale to the newer scale, using
[`T90fromT68()`](https://dankelley.github.io/oce/reference/T90fromT68.md)
before doing calculations that are expressed in terms of the `"ITS-90"`
scale. This is taken care of by retrieving temperatures with the
accessor function, e.g. writing `A[["temperature"]]` will either
retrieve the stored values (if the scale is ITS-90) or converted values
(if the scale is IPTS-68). Even though this procedure should work, users
who really care about the details of their data are well-advised to do a
couple of tests after examining the first data line of their data file
in an editor. Note that reading a file that contains IPTS-68
temperatures produces a warning.

## References

1.  The Sea-Bird SBE 19plus profiler is described at
    `http://www.seabird.com/products/spec_sheets/19plusdata.htm`. Some
    more information is given in the Sea-Bird data-processing manual
    (next item).

2.  A SBE data processing manual was once at
    `http://www.seabird.com/document/sbe-data-processing-manual`, but as
    of summer 2018, this no longer seems to be provided by SeaBird. A
    web search will turn up copies of the manual that have been put
    online by various research groups and data-archiving agencies. As of
    2018-07-05, the latest version was named
    `SBEDataProcessing_7.26.4.pdf` and had release date 12/08/2017, and
    this was the reference version used in coding `oce`.

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
[`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`ctdRepair()`](https://dankelley.github.io/oce/reference/ctdRepair.md),
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md),
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md),
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.odv()`](https://dankelley.github.io/oce/reference/read.ctd.odv.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

Other functions that read ctd data:
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md)

## Author

Dan Kelley and Clark Richards, along with William Kumler.

## Examples

``` r
# Example 1: default variable renaming
f <- system.file("extdata", "ctd.cnv.gz", package = "oce")
d1 <- read.ctd(f)
#> Warning: suspicious startTime 1903-10-15 11:38:38 changed to 2003-10-15 11:38:38; see 'start_time' in file header
#> Warning: file has temperature in IPTS-68 so this is stored as-is, but note that [["temperature"]] and sw* functions autoconvert to ITS-90 to match modern conventions

# Example 2: handle variable renaming after reading
d2 <- read.ctd(f, rename = FALSE) |> oceRename("sbe")
#> Warning: suspicious startTime 1903-10-15 11:38:38 changed to 2003-10-15 11:38:38; see 'start_time' in file header
```
