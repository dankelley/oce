# Read an adp File in Teledyne/RDI Format

Read a Teledyne/RDI ADCP file (called 'adp' in oce). This can handle a
variety of file/instrument types, by recognizing telltale byte sequences
in the data. The scope is limited to types that are documented
adequately in Teledyne/RDI manuals. In some instances, the manuals
provide some information but not enough to enable inclusion here, for
example in the case for wave data (see
<https://github.com/dankelley/oce/issues/2216>).

## Usage

``` r
read.adp.rdi(
  file,
  from,
  to,
  by,
  tz = getOption("oceTz"),
  longitude = NA,
  latitude = NA,
  type = c("workhorse"),
  which,
  encoding = NA,
  monitor = FALSE,
  despike = FALSE,
  processingLog,
  testing = FALSE,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load. (For `read.adp.sontek.serial`, this is generally a list of
  files, which will be concatenated.)

- from:

  indication of the first profile to read. This can be an integer, the
  sequence number of the first profile to read, or a POSIXt time before
  which profiles should be skipped, or a character string that converts
  to a POSIXt time (assuming UTC timezone). See “Examples”, and make
  careful note of the use of the `tz` argument. If `from` is not
  supplied, it defaults to 1.

- to:

  an optional indication of the last profile to read, in a format as
  described for `from`. As a special case, `to=0` means to read the file
  to the end. If `to` is not supplied, then it defaults to 0.

- by:

  an optional indication of the stride length to use while walking
  through the file. If this is an integer, then `by-1` profiles are
  skipped between each pair of profiles that is read, e.g. the default
  `by=1` means to read all the data. (For RDI files *only*, there are
  some extra features to avoid running out of memory; see “Memory
  considerations”.)

- tz:

  character string indicating time zone to be assumed in the data.

- longitude:

  optional signed number indicating the longitude in degrees East.

- latitude:

  optional signed number indicating the latitude in degrees North.

- type:

  character string indicating the type of instrument.

- which:

  optional character value. If this is `"??"` then the only other
  parameters that are examined are `file` and `debug`, and
  `read.adp.rdi()` does not try to read the detailed data. Instead, all
  it does is to identify the starting locations of the
  profiles/ensembles in the file, reporting these in a data frame that
  also includes the size of the profile/ensemble and the time when the
  measurement was made. This information may prove useful to analysts
  who are doing low-level work.

- encoding:

  ignored.

- monitor:

  boolean value indicating whether to indicate the progress of reading
  the file, by using
  [`txtProgressBar()`](https://rdrr.io/r/utils/txtProgressBar.html) or
  otherwise. The value of `monitor` is changed to `FALSE` automatically,
  for non-interactive sessions.

- despike:

  if `TRUE`,
  [`despike()`](https://dankelley.github.io/oce/reference/despike.md)
  will be used to clean anomalous spikes in heading, etc.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

- testing:

  logical value (IGNORED).

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  optional additional arguments that some (but not all) `read.adp.*()`
  functions pass to lower-level functions.

## Value

An [adp](https://dankelley.github.io/oce/reference/adp-class.md) object.
The contents of that object make sense for the particular instrument
type under study, e.g. if the data file contains NMEA strings, then
navigational data will be stored in an item called `nmea` in the `data`
slot).

## Details

If a heading bias had been set with the `EB` command during the setup
for the deployment, then a heading bias will have been stored in the
file's header. This value is stored in the object's metadata as
`metadata$heading.bias`. **Importantly**, this value is subtracted from
the headings stored in the file, and the result of this subtraction is
stored in the objects heading value (in `data$heading`). It should be
noted that `read.adp.rdi()` was tested for firmware version 16.30. For
other versions, there may be problems. For example, the serial number is
not recognized properly for version 16.28.

In Teledyne/RDI ADP data files, velocities are coded to signed 2-byte
integers, with a scale factor being used to convert to velocity in
metres per second. These two facts control the maximum recordable
velocity and the velocity resolution, values that may be retrieved for
an ADP object name `d` with `d[["velocityMaximum"]]` and
`d[["velocityResolution"]]`.

## Handling of old file formats

Early PD0 file formats stored the year of sampling with a different base
year than that used in modern files. To accommodate this, `read.adp.rdi`
examines the inferred year, and if it is greater than 2050, then 100
years are subtracted from the time. This offset was inferred by tests
with sample files, but *not* from RDI documentation, so it is somewhat
risky. If the authors can find RDI documentation that indicates the
condition in which this century offset is required, then a change will
be made to the code. Even if not, the method should not cause problems
for a long time.

## Names of items in data slot

The names of items in the `data` slot are below. Not all items are
present for ll file varieties; use e.g. `names(d[["data"]])` to
determine the names used in an object named `d`. In this list, items are
either a vector (with one sample per time of measurement), a
[matrix](https://rdrr.io/r/base/matrix.html) with first index for time
and second for bin number, or an
[array](https://rdrr.io/r/base/array.html) with first index for time,
second for bin number, and third for beam number. Items are of vector
type, unless otherwise indicated.

|  |  |
|----|----|
| **Item** | **Meaning** |
| `a` | signal amplitude array (units?) |
| `ambientTemp` | ambient temperature (degC) |
| `attitude` | attitude (deg) |
| `attitudeTemp` | (FIXME add a description here) |
| `avgMagnitudeVelocityEast` | (FIXME add a description here) |
| `avgMagnitudeVelocityNorth` | (FIXME add a description here) |
| `avgSpeed` | (FIXME add a description here) |
| `avgTrackMagnetic` | (FIXME add a description here) |
| `avgTrackTrue` | (FIXME add a description here) |
| `avgTrueVelocityEast` | (FIXME add a description here) |
| `avgTrueVelocityNorth` | (FIXME add a description here) |
| `br` | bottom range matrix (m) |
| `bv` | bottom velocity matrix (m/s) |
| `contaminationSensor` | (FIXME add a description here) |
| `depth` | depth (m) |
| `directionMadeGood` | (FIXME add a description here) |
| `distance` | (FIXME add a description here) |
| `firstLatitude` | latitude at start of profile (deg) |
| `firstLongitude` | longitude at start of profile (deg) |
| `firstTime` | (FIXME add a description here) |
| `g` | data goodness matrix (units?) |
| `heading` | instrument heading (degrees) |
| `headingStd` | instrument heading std-dev (deg) |
| `ISMvalid` | indication of whether ISM data are in the file |
| `ISMacc` | ISM acceleration, in 3 directions |
| `ISMmag` | ISM earth magnetic field, in 3 directions |
| `lastLatitude` | latitude at end of profile (deg) |
| `lastLongitude` | longitude at end of profile (deg) |
| `lastTime` | (FIXME add a description here) |
| `numberOfHeadingSamplesAveraged` | (FIXME add a description here) |
| `numberOfMagneticTrackSamplesAveraged` | (FIXME add a description here) |
| `numberOfPitchRollSamplesAveraged` | (FIXME add a description here) |
| `numberOfSpeedSamplesAveraged` | (FIXME add a description here) |
| `numberOfTrueTrackSamplesAveraged` | (FIXME add a description here) |
| `pitch` | instrument pitch (deg) |
| `pitchStd` | instrument pitch std-dev (deg) |
| `pressure` | pressure (dbar) |
| `pressureMinus` | (FIXME add a description here) |
| `pressurePlus` | (FIXME add a description here) |
| `pressureStd` | pressure std-dev (dbar) |
| `primaryFlags` | (FIXME add a description here) |
| `q` | data quality array |
| `roll` | instrument roll (deg) |
| `rollStd` | instrument roll std-dev (deg) |
| `salinity` | salinity |
| `shipHeading` | ship heading (deg) |
| `shipPitch` | ship pitch (deg) |
| `shipRoll` | ship roll (deg) |
| `soundSpeed` | sound speed (m/s) |
| `speedMadeGood` | speed over ground (?) (m/s) |
| `speedMadeGoodEast` | (FIXME add a description here) |
| `speedMadeGoodNorth` | (FIXME add a description here) |
| `temperature` | temperature (degC) |
| `time` | profile time (POSIXct) |
| `v` | velocity array (m/s) |
| `xmitCurrent` | transmit current (unit?) |
| `xmitVoltage` | transmit voltage |

## Memory considerations

For `RDI` files only, and only in the case where `by` is not specified,
an attempt is made to avoid running out of memory by skipping some
profiles in large input files. This only applies if `from` and `to` are
both integers; if they are times, none of the rest of this section
applies.

A key issue is that RDI files store velocities in 2-byte values, which
is not a format that R supports. These velocities become 8-byte
(numeric) values in R. Thus, the R object created by `read.adp.rdi` will
require more memory than that of the data file. A scale factor can be
estimated by ignoring vector quantities (e.g. time, which has just one
value per profile) and concentrating on matrix properties such as
velocity, backscatter, and correlation. These three elements have equal
dimensions. Thus, each 4-byte slide in the data file (2 bytes + 1 byte +
1 byte) corresponds to 10 bytes in the object (8 bytes + 1 byte + 1
byte). Rounding up the resultant 10/4 to 3 for safety, we conclude that
any limit on the size of the R object corresponds to a 3X smaller limit
on file size.

Various things can limit the size of objects in R, but a strong upper
limit is set by the space the operating system provides to R. The
least-performant machines in typical use appear to be Microsoft-Windows
systems, which limit R objects to about 2e6 bytes (see
`?Memory-limits`). Since R routinely duplicates objects for certain
tasks (e.g. for call-by-value in function evaluation), `read.adp.rdi`
uses a safety factor in its calculation of when to auto-decimate a file.
This factor is set to 3, based partly on the developers' experience with
datasets in their possession. Multiplied by the previously stated safety
factor of 3, this suggests that the 2 GB limit on R objects corresponds
to approximately a 222 MB limit on file size. In the present version of
`read.adp.rdi`, this value is lowered to 200 MB for simplicity. Larger
files are considered to be "big", and are decimated unless the user
supplies a value for the `by` argument.

The decimation procedure has two cases.

1.  If `from=1` and `to=0` (or if neither `from` or `to` is given), then
    the intention is to process the full span of the data. If the input
    file is under 200 MB, then `by` defaults to 1, so that all profiles
    are read. For larger files, `by` is set to the
    [`ceiling()`](https://rdrr.io/r/base/Round.html) of the ratio of
    input file size to 200 MB.

2.  If `from` exceeds 1, and/or `to` is nonzero, then the intention is
    to process only an interior subset of the file. In this case, `by`
    is calculated as the
    [`ceiling()`](https://rdrr.io/r/base/Round.html) of the ratio of
    `bbp*(1+to-from)` to 200 MB, where `bbp` is the number of file bytes
    per profile. Of course, `by` is set to 1, if this ratio is less than
    1.

If the result of these calculations is that `by` exceeds 1, then
messages are printed to alert the user that the file will be decimated,
and also `monitor` is set to `TRUE`, so that a textual progress bar is
shown (if the session is interactive).

## Development Notes

An important part of the work of this function is to recognize what will
be called "data chunks" by two-byte ID sequences. This function is
developed in a practical way, with emphasis being focussed on data files
in the possession of the developers. Since Teledyne-RDI tends to
introduce new ID codes with new instruments, that means that
`read.adp.rdi` may not work on recently-developed instruments.

The following two-byte ID codes are recognized by `read.adp.rdi` at this
time (with bytes listed in natural order, LSB byte before MSB). Items
preceded by an asterisk are recognized, but not handled, and so produce
a warning.

|  |  |  |  |
|----|----|----|----|
|  | **Byte 1** | **Byte 2** | **Meaning** |
|  | 0x00 | 0x01 | velocity |
|  | 0x00 | 0x01 | velocity |
|  | 0x00 | 0x02 | correlation |
|  | 0x00 | 0x03 | echo intensity |
|  | 0x00 | 0x04 | percent good |
|  | 0x00 | 0x06 | bottom track |
|  | 0x00 | 0x0a | Sentinel vertical beam velocity |
|  | 0x00 | 0x0b | Sentinel vertical beam correlation |
|  | 0x00 | 0x0c | Sentinel vertical beam amplitude |
|  | 0x00 | 0x0d | Sentinel vertical beam percent good |
|  | 0x00 | 0x20 | VMDASS |
|  | 0x00 | 0x30 | Binary Fixed Attitude header |
|  | 0x00 | 0x32 | Sentinel transformation matrix |
|  | 0x00 | 0x0a | Sentinel data |
|  | 0x00 | 0x0b | Sentinel correlation |
|  | 0x00 | 0x0c | Sentinel amplitude |
|  | 0x00 | 0x0d | Sentinel percent good |
|  | 0x01 | 0x0f | Perhaps something to do with V series and 4-beam |
|  | 0x01 | 0x59 | ISM (earth magnetic field and sensor acceleration) |
|  | 0x0C | 0x02 | Ambient sound |

Lacking a comprehensive Teledyne-RDI listing of ID codes, the authors
have cobbled together a listing from documents to which they have
access, as follows.

- Table 33 of reference 3 lists codes as follows:

  |                 |                      |                             |
  |-----------------|----------------------|-----------------------------|
  | **Standard ID** | **Standard plus 1D** | **DESCRIPTION**             |
  | MSB LSB         | MSB LSB              |                             |
  | — —             | — —                  |                             |
  | 7F 7F           | 7F 7F                | Header                      |
  | 00 00           | 00 01                | Fixed Leader                |
  | 00 80           | 00 81                | Variable Leader             |
  | 01 00           | 01 01                | Velocity Profile Data       |
  | 02 00           | 02 01                | Correlation Profile Data    |
  | 03 00           | 03 01                | Echo Intensity Profile Data |
  | 04 00           | 04 01                | Percent Good Profile Data   |
  | 05 00           | 05 01                | Status Profile Data         |
  | 06 00           | 06 01                | Bottom Track Data           |
  | 20 00           | 20 00                | Navigation                  |
  | 30 00           | 30 00                | Binary Fixed Attitude       |
  | 30 40-F0        | 30 40-F0             | Binary Variable Attitude    |

- Table 6 on p90 of reference 4 lists "Fixed Leader Navigation" ID codes
  (none of which are handled by `read.adp.rdi` yet) as follows (the
  format is reproduced literally; note that e.g. 0x2100 is 0x00,0x21 in
  the oce notation):

  |        |                             |
  |--------|-----------------------------|
  | **ID** | **Description**             |
  | 0x2100 | \$xxDBT                     |
  | 0x2101 | \$xxGGA                     |
  | 0x2102 | \$xxVTG                     |
  | 0x2103 | \$xxGSA                     |
  | 0x2104 | \$xxHDT, \$xxHGD or \$PRDID |

  and following pages in that manual reveal the following meanings

  |            |                                   |
  |------------|-----------------------------------|
  | **Symbol** | **Meaning**                       |
  | `DBT`      | depth below transducer            |
  | `GGA`      | global positioning system         |
  | `VTA`      | track made good and ground speed  |
  | `GSA`      | GPS DOP and active satellites     |
  | `HDT`      | heading, true                     |
  | `HDG`      | heading, deviation, and variation |
  | `PRDID`    | heading, pitch and roll           |

## Error recovery

Files can sometimes be corrupted, and `read.adp.rdi` has ways to handle
two types of error that have been noticed in files supplied by users.

1.  There are two bytes within each ensemble that indicate the number of
    bytes to check within that ensemble, to get the checksum. Sometimes,
    those two bytes can be erroneous, so that the wrong number of bytes
    are checked, leading to a failed checksum. As a preventative
    measure, `read.adp.rdi` checks the stated ensemble length, whenever
    it detects a failed checksum. If that length agrees with the length
    of the most recent ensemble that had a good checksum, then the
    ensemble is declared as faulty and is ignored. However, if the
    length differs from that of the most recent accepted ensemble, then
    `read.adp.rdi` goes back to just after the start of the ensemble,
    and searches forward for the next two-byte pair, namely `0x7f 0x7f`,
    that designates the start of an ensemble. Distinct notifications are
    given about these two cases, and they give the byte numbers in the
    original file, as a way to help analysts who want to look at the
    data stream with other tools.

2.  At the end of an ensemble, the next two characters ought to be
    `0x7f 0x7f`, and if they are not, then the next ensemble is faulty.
    If this error occurs, `read.adp.rdi` attempts to recover by
    searching forward to the next instance of this two-byte pair,
    discarding any information that is present in the mangled ensemble.

In each of these cases, warnings are printed about ensembles that seem
problematic. Advanced users who want to diagnose the problem further
might find it helpful to examine the original data file using other
tools. To this end, `read.adp.rdi` inserts an element named
`ensembleInFile` into the `metadata` slot. This gives the starting byte
number of each inferred ensemble within the original data file. For
example, if `d` is an object read with `read.adp.rdi`, then using

    plot(d[["time"]][-1], diff(d[["ensembleInFile"]]))

can be a good way to narrow in on problems.

## Changes

- The `bq` (bottom-track quality) field was called `bc` until
  2023-02-09. See https://github.com/dankelley/oce/issues/2039 for
  discussion.

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

## References

1.  Teledyne-RDI, 2007. *WorkHorse commands and output data format.* P/N
    957-6156-00 (November 2007). (Section 5.3 h details the binary
    format, e.g. the file should start with the byte `0x7f` repeated
    twice, and each profile starts with the bytes `0x80`, followed by
    `0x00`, followed by the sequence number of the profile, represented
    as a little-endian two-byte short integer. `read.adp.rdi` uses these
    sequences to interpret data files.)

2.  Teledyne RD Instruments, 2015. *V Series monitor, sentinel Output
    Data Format.* P/N 95D-6022-00 (May 2015). `SV_ODF_May15.pdf`

3.  Teledyne RD Instruments, 2014. *Ocean Surveyor / Ocean Observer
    Technical Manual.* P/N 95A-6012-00 (April 2014). `OS_TM_Apr14.pdf`

4.  Teledyne RD Instruments, 2001. *WinRiver User's Guide International
    Version.* P/N 957-6171-00 (June 2001)
    `WinRiver User Guide International Version.pdf.pdf`

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
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
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

Other functions that read adp data:
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md)

## Author

Dan Kelley and Clark Richards

## Examples

``` r
adp <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package = "oce"))
summary(adp)
#> ADP Summary
#> -----------
#> 
#> * Filename:          "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpdNg0iG/temp_libpath32578b0e13b/oce/extdata/adp_rdi.000"
#> * Instrument:        adcp
#> * Manufacturer:      teledyne rdi
#> * Serial number:     unknown
#> * Firmware:          16 (major), 28 (minor)
#> * Firmware:          16.28
#> * Cell Size:         0.50 m
#> * Beam Angle:        20 deg
#> * Location:          unknown latitude, unknown longitude
#> * Frequency:         600 kHz
#> * Ensemble Numbers:   1, 2, ..., 8, 9
#> * Transformation matrix::
#>      1.462  -1.462   0.000   0.000
#>      0.000   0.000  -1.462   1.462
#>      0.266   0.266   0.266   0.266
#>      1.034   1.034  -1.034  -1.034
#> * Time: 2008-06-25 10:00:00 to 2008-06-25 10:01:20 (9 samples, mean increment 10 s)
#> * Data Overview
#> 
#>                              Min.       Mean       Max.       Dim.     NAs
#>     v [m/s]                  -0.416     0.011534   0.406      "9x84x4" 0  
#>     q                        NA         NA         NA         "9x84x4" 0  
#>     a                        NA         NA         NA         "9x84x4" 0  
#>     g                        NA         NA         NA         "9x84x4" 0  
#>     distance [m]             2.23       22.98      43.73      84       0  
#>     time                     1214388000 1214388040 1214388080 9        0  
#>     pressure [dbar]          -0.274     -0.23489   -0.193     9        0  
#>     temperature [°C, ITS-90] 12.06      12.082     12.11      9        0  
#>     salinity [PSS-78]        35         35         35         9        0  
#>     depth [m]                0          0          0          9        0  
#>     soundSpeed [m/s]         1497       1497       1497       9        0  
#>     heading [°]              276.39     277.14     278.14     9        0  
#>     pitch [°]                1.1209     1.2033     1.4212     9        0  
#>     roll [°]                 -2.49      -2.4033    -2.35      9        0  
#>     headingStd [°]           0          0.22222    1          9        0  
#>     pitchStd [°]             0.1        0.11111    0.2        9        0  
#>     rollStd [°]              0          0.088889   0.1        9        0  
#>     pressureStd              76         100        135        9        0  
#>     xmitCurrent              61         61         61         9        0  
#>     xmitVoltage              155        155        155        9        0  
#>     ambientTemp              103        103        103        9        0  
#>     pressurePlus             77         77         77         9        0  
#>     pressureMinus            76         76.778     77         9        0  
#>     attitudeTemp             101        101        101        9        0  
#>     attitude [°]             130        130        130        9        0  
#>     contaminationSensor      159        159        159        9        0  
#>     ISMvalid                 NA         NA         NA         0        0  
#>     ISMacc                   NA         NA         NA         0        0  
#>     ISMmag                   NA         NA         NA         0        0  
#>     ambientSound             NA         NA         NA         0        0  
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 20:57:50 UTC: `read.adp.rdi(file = system.file("extdata", "adp_rdi.000", package = "oce"))`
```
