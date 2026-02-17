# Read an adp File in Nortek Aquadopp Format

The R code is based on information in the Nortek System Integrator Guide
(2017), postings on the Nortek “knowledge center” discussion board, and
discussions with Nortek engineers (Dec. 2020).

## Usage

``` r
read.aquadopp(
  file,
  from = 1,
  to,
  by = 1,
  tz = getOption("oceTz"),
  longitude = NA,
  latitude = NA,
  type = "aquadopp",
  orientation,
  distance,
  monitor = FALSE,
  despike = FALSE,
  encoding = NA,
  processingLog,
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

  Either "aquadopp" for a standard aquadopp file (the default), or
  "aquadoppPlusMagnetometer" for a file which includes the raw
  magnetometer data.

- orientation:

  Optional character string specifying the orientation of the sensor,
  provided for those cases in which it cannot be inferred from the data
  file. The valid choices are `"upward"`, `"downward"`, and
  `"sideward"`.

- distance:

  Optional vector holding the distances of bin centres from the sensor.
  This argument is ignored except for Nortek profilers, and need not be
  given if the function determines the distances correctly from the
  data. The problem is that the distance is poorly documented in the
  Nortek System Integrator Guide (2008 edition, page 31), so the
  function must rely on word-of-mouth formulae that do not work in all
  cases.

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

- encoding:

  ignored.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

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

1.  Information on Nortek profilers (including the System Integrator
    Guide, which explains the data format byte-by-byte) is available at
    `https://www.nortekusa.com/`. (One must join the site to see the
    manuals.)

2.  The Nortek Knowledge Center
    `https://www.nortekusa.com/en/knowledge-center` may be of help if
    problems arise in dealing with data from Nortek instruments.

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
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
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
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md)

## Author

Dan Kelley and Clark Richards
