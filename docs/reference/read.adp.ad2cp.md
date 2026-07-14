# Read an adp File in Nortek AD2CP Format

This function is under active development and may change without notice.
In contrast with other `oce` reading functions, `read.adp.ad2cp()`
focusses just on one data type within the source file. Another
difference is that it can either return an object holding the data or
just a data frame holding a description of the data types in the file;
indeed, the latter is the default. See “Details” for more on the reasons
for these departures from the usual `oce` pattern.

## Usage

``` r
read.adp.ad2cp(
  file,
  from = 1L,
  to = 0L,
  by = 1L,
  dataType = NULL,
  dataSet = 1L,
  tz = getOption("oceTz"),
  longitude = NA,
  latitude = NA,
  plan,
  TOC = FALSE,
  debug = getOption("oceDebug"),
  orientation,
  distance,
  monitor,
  despike,
  ...
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- from:

  an integer indicating the index number of the first record to read.
  This must equal 1, for this version of `read.adp.ad2cp`. (If not
  provided, `from` defaults to 1.)

- to:

  an integer indicating the final record to read. If `to` is 0L, which
  is the default, then the value is changed internally to 1e9, and
  reading stops at the end of the file.

- by:

  ignored.

- dataType:

  an indication of the data type to be extracted. If this is NULL (the
  default) then `read.adp.ad2cp()` returns a data frame indicating the
  data type occurrence rate in the file. Otherwise, `dataType` must be
  either a numeric or character value (see “Details”). In the numeric
  case, which includes both base-10 numbers and `raw` values, `dataType`
  is converted to an integer that is taken to indicate the data type via
  ID. The permitted values follow the Nortek convention, a summary of
  which is shown the table at the start of the “Details” section. In the
  character case, it must be a string taken from that same table.

- dataSet:

  a positive integer that indicates which of the possibly several data
  sets stored within a file is to be focussed upon. By default, the
  first data set is chosen. Note that data sets are found by trying to
  match each text data chunk against the regular expression
  `"^GETCLOCKSTR,TIME="`.

- tz:

  a character value indicating time zone. This is used in interpreting
  times stored in the file.

- longitude, latitude:

  numerical values indicating the observation location.

- plan:

  optional integer specifying which 'plan' to focus on (see

- TOC:

  a logical value. If this is FALSE (the default) then the other
  parameters of the function are used to select data from the indicated
  `filename`, and an
  [adp](https://dankelley.github.io/oce/reference/adp-class.md) object
  is returned. However, if `TOC` is TRUE, then a dataframe detailing the
  number of entries in each of the datasets stored within the file is
  returned.

- debug:

  an integer value indicating the level of debugging. Set to 1 to get a
  moderate amount of debugging information, from the R code only, to 2
  to get some debugging information from the C++ code that is used to
  parse the data chunks, or to 3 for intensive debugging at both levels.

- orientation, distance, monitor, despike:

  ignored, provided only for calling compatibility with other functions
  that read
  [adp](https://dankelley.github.io/oce/reference/adp-class.md) files. A
  warning is issued if any of these is supplied in a call to
  `read.adp.ad2cp()`.

- ...:

  ignored parameters that might be passed to `read.adp.ad2cp()` by
  [`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md).

## Value

`read.adp.ad2cp()` returns either an
[adp](https://dankelley.github.io/oce/reference/adp-class.md) object or
the number of data sets within the file, according to the value of
`TOC`.

## Details

Why does `read.adp.ad2cp()` focus only on parts of the data file? The
answer lies in the AD2CP format itself, which may combine data subsets
of such differing natures as to break with the `oce` system of pairing a
`metadata` slot with a `data` slot. For example, in a conventional ADP
dataset, the `metadata` slot has items for the sampling times, the
number of beams, the blanking distance, the cell size, the number of
cells, etc. Such items have a natural pairing with elements of the
`data` slot, and `oce` uses this pairing in constructing plots and other
items. However, an AD2CP file might combine such data with echosounder
measurements, and these will have different values for number of beams
and so forth. This poses a challenge in naming conventions within the
`oce` object, with ripple effects for plotting and data access. Those
ripple effects would extend beyond `oce` itself to user code. To avoid
such problems, `read.adp.ad2cp()` is designed to focus on one data type
at a time, relying on users to keep track of the resultant object,
perhaps to combine it with other objects from within the AD2CP file or
other files, in the normal R manner.

The permitted values for `dataType` are shown in the table below; the
`dataType` argument of `read.adp.ad2cp()` may be given as listed in any
of the first 3 columns of this table.

|            |                |                       |       |
|------------|----------------|-----------------------|-------|
| code (raw) | code (integer) | oce name              | notes |
| ———-       | ————–          | ——————-               | ——    |
| `0x15`     | 21             | `burst`               | \-    |
| `0x16`     | 22             | `average`             | \-    |
| `0x17`     | 23             | `bottomTrack`         | 1     |
| `0x18`     | 24             | `interleavedBurst`    | \-    |
| `0x1a`     | 26             | `burstAltimeterRaw`   | 2     |
| `0x1b`     | 27             | `DVLBottomTrack`      | \-    |
| `0x1c`     | 28             | `echosounder`         | \-    |
| `0x1d`     | 29             | `DVLWaterTrack`       | \-    |
| `0x1e`     | 30             | `altimeter`           | \-    |
| `0x1f`     | 31             | `averageAltimeterRaw` | 3     |
| `0x23`     | 35             | `echosounderRaw`      | \-    |
| `0x24`     | 36             | `echosounderRawTx`    | 4     |
| `0x30`     | 48             | `waves`               | 5     |
| `0xa0`     | 160            | `text`                | 6     |

Note 1. Tentative support for reading `dataType=bottomTrack` was added
in April of 2026. The problem with this `dataType` is that the most
recent Nortek manuals (e.g. Reference 3) do not provide any
documentation, and there are contradictions between an older manual
(Reference 4) and information provided in a Nortek email to Dan Kelley
and Clark Richards, dated 2026-03-24 (Reference 5). The known problems
include the following (see
`https://github.com/dankelley/oce/issues/2368` for discussion), (1) The
`ensembleCounter` field is certainly incorrect, as it cycles from 1 to
60 in test files, instead of increasing monotonically. (2) The byte that
is supposed to hold both the coordinate system and the number of beams
seems not to be configured identically across the handful of test files
used during oce development. As a remedy, the function tries to detect
faulty values, and switches to using the header record if so (issuing a
warning so the user will know that assumptions have been made).

Note 2. In reading a file with `burstAltimeterRaw` data components, a
potential problem was noticed with that the part of the file that
indicates the number of altimeter samples (called `NSAMP` in the Nortek
documentation). The stated value was 2 times the value held in the
header (text) portion of the file, and reading the larger value created
a matrix that had the upper half filled with odd striping patterns.
Therefore, the function checks the two indications of length, and uses
the value in the text block if they disagree. (See
https://github.com/dankelley/oce/issues/2326.)

Note 3. In April 2026, the name `averageAltimeter` (hex code 0x14) was
changed to `averageAltimeterRaw`, to be more consistent with names used
in Reference 3. The old name will still work, but a warning will be
issued indicating that the name was automatically replaced with the new
name.

Note 4: Code 0x24 (`echosounderRawTx`) has some coding done, but it is
untested, as the developers lack a data file exemplar. For now, this
data type is read as though it were 0x23, which is likely to produce
poor results or cause errors in processing.

Note 5: Code 0x30 (`waves`) is recognized but not handled yet.

Note 6: Code 0xa0 (`text`) holds a text string that defines the settings
used in creating the file. This can be quite helpful in debugging and
analysis.

## How the binary file is decoded

This file type, like other acoustic-Doppler types, is read with a hybrid
R/C++ system, for efficiency. The processing steps are sketched below,
for users who want to inspect the code or build upon it.

1.  In R, [`readBin()`](https://rdrr.io/r/base/readBin.html) is used to
    insert the file contents into a
    [vector](https://rdrr.io/r/base/vector.html) of type `raw`.

2.  In C++, this raw vector is scanned byte by byte, to find the
    starting indices of data "chunks", or subsections of the data that
    correspond to individual sampling times. Checksum computations are
    also done at this stage, to detect possible data corruption.
    Warnings are issued for any bad chunks, and they are skipped in
    further processing. The valid starting points are then passed back
    to R as a [vector](https://rdrr.io/r/base/vector.html) of type
    `integer`.

3.  In R, [`readBin()`](https://rdrr.io/r/base/readBin.html) is used to
    read the components of each chunk. For speed, this is done in a
    vectorized fashion. For example, all the velocities in the whole
    file are read in a single call to
    [`readBin()`](https://rdrr.io/r/base/readBin.html). This process is
    done for each of the data fields that are to be handled.
    Importantly, these
    [`readBin()`](https://rdrr.io/r/base/readBin.html) calls are
    tailored to the data, using values of the `size`, `endian` and
    `signed` parameters that are tailored to the structure of the given
    component. Scaling factors are then applied as required, to convert
    the components to physical units.

4.  Finally, in R, the acquired items are inserted into the `data` or
    `metadata` slot of the return value, according to oce convention.

## Problems

1.  It is unclear from the manufacturer's manuals whether `NSAMP` is a
    2-byte value (as stated in old manuals) or a 4-byte value (as in a
    manual available in or around June 2025). The present function
    assumes a 4-byte value, which works with a test file available to
    the authors. The manufacturer has been asked for clarity on this;
    see https://github.com/dankelley/oce/issues/2326.

## References

1.  Nortek AS. “Integrator's Guide: Signature.” Nortek AS, April
    30, 2025.
    `https://support.nortekgroup.com/hc/en-us/article_attachments/19830760385436`
    (this link failed in a test on 2025-07-30).

2.  Nortek AS. “Signature Integration 55\|250\|500\|1000kHz
    (2024.1),” 2024. (This was once at
    `https://support.nortekgroup.com/hc/en-us/articles/360029513952-Integrators-Guide-Signature`
    but a test on 2025-05-26 revealed that this link no longer worked.)

3.  Nortek AS. “Integrator's Guide Signature.” March 5, 2026.
    <https://support.nortekgroup.com/hc/en-us/article_attachments/25920785046428>.

4.  Nortek AS. “Signature Integration 55\|250\|500\|1000kHz (2017).”
    February 10, 2017.
    <https://assets.nortekgroup.com/software/N3015-007-Integrators-Guide-AD2CP_1018.pdf>.

5.  <https://github.com/dankelley/oce/issues/2368>

## See also

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpConvertRawToNumeric()`](https://dankelley.github.io/oce/reference/adpConvertRawToNumeric.md),
[`adpEnsembleAverage()`](https://dankelley.github.io/oce/reference/adpEnsembleAverage.md),
[`adpFlagPastBoundary()`](https://dankelley.github.io/oce/reference/adpFlagPastBoundary.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`as.adp()`](https://dankelley.github.io/oce/reference/as.adp.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`beamToXyzAdp()`](https://dankelley.github.io/oce/reference/beamToXyzAdp.md),
[`beamToXyzAdpAD2CP()`](https://dankelley.github.io/oce/reference/beamToXyzAdpAD2CP.md),
[`beamToXyzAdv()`](https://dankelley.github.io/oce/reference/beamToXyzAdv.md),
[`beamUnspreadAdp()`](https://dankelley.github.io/oce/reference/beamUnspreadAdp.md),
[`binmapAdp()`](https://dankelley.github.io/oce/reference/binmapAdp.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdp()`](https://dankelley.github.io/oce/reference/enuToOtherAdp.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md),
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subtractBottomVelocity()`](https://dankelley.github.io/oce/reference/subtractBottomVelocity.md),
[`summary,adp-method`](https://dankelley.github.io/oce/reference/summary-adp-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdp()`](https://dankelley.github.io/oce/reference/toEnuAdp.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdp()`](https://dankelley.github.io/oce/reference/xyzToEnuAdp.md),
[`xyzToEnuAdpAD2CP()`](https://dankelley.github.io/oce/reference/xyzToEnuAdpAD2CP.md)

Other things related to ad2cp data:
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md)

Other functions that read adp data:
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md)

## Author

Dan Kelley and Clark Richards

## Examples

``` r
library(oce)
# You can run this within the oce directory, if you clone from github.
file <- "tests/testthat/local_data/ad2cp/S102791A002_Barrow_v2.ad2cp"
if (file.exists(file)) {
    library(oce)
    print(read.oce(file, TOC = TRUE))
    d <- read.oce(file, dataType = "burstAltimeterRaw")
    imagep(d[["time"]], d[["altimeterRawDistance"]], d[["altimeterRawSamples"]])
}
```
