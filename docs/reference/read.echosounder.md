# Read an echosounder File

Reads a biosonics echosounder file. This function was written for and
tested with single-beam, dual-beam, and split-beam Biosonics files of
type V3, and may not work properly with other file formats.

## Usage

``` r
read.echosounder(
  file,
  channel = 1,
  soundSpeed,
  tz = getOption("oceTz"),
  encoding = NA,
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- channel:

  sequence number of channel to extract, for multi-channel files.

- soundSpeed:

  sound speed, in m/s. If not provided, this is calculated using
  [`swSoundSpeed`](https://dankelley.github.io/oce/reference/swSoundSpeed.md)`(35,15,30,eos="unesco")`.
  (In theory, it could be calculated using the temperature and salinity
  that are stored in the data file, but these will just be nominal
  values, anyway.

- tz:

  character string indicating time zone to be assumed in the data.

- encoding:

  ignored.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- processingLog:

  if provided, the action item to be stored in the log, typically only
  provided for internal calls.

## Value

An
[echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
object.

## Bugs

Only the amplitude information (in counts) is determined. A future
version of this function may provide conversion to dB, etc. The handling
of dual-beam and split-beam files is limited. In the dual-beam cse, only
the wide beam signal is processed (I think ... it could be the narrow
beam, actually, given the confusing endian tricks being played). In the
split-beam case, only amplitude is read, with the x-axis and y-axis
angle data being ignored.

## References

Various echosounder instruments provided by BioSonics are described at
the company website, `https://www.biosonicsinc.com/`. The document
listed as reference 1 below was provided to the author of this function
in November 2011, which suggests that the data format was not changed
since July 2010.

1.  Biosonics, 2010. DT4 Data File Format Specification. BioSonics
    Advanced Digital Hydroacoustics. July, 2010. SOFTWARE AND
    ENGINEERING LIBRARY REPORT BS&E-2004-07-0009-2.0.

## See also

The documentation for
[echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
explains the structure of `ctd` objects, and also outlines the other
functions dealing with them.

Other things related to echosounder data:
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
`[[<-,echosounder-method`,
[`as.echosounder()`](https://dankelley.github.io/oce/reference/as.echosounder.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`echosounder-class`](https://dankelley.github.io/oce/reference/echosounder-class.md),
[`findBottom()`](https://dankelley.github.io/oce/reference/findBottom.md),
[`plot,echosounder-method`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`summary,echosounder-method`](https://dankelley.github.io/oce/reference/summary-echosounder-method.md)

## Author

Dan Kelley, with help from Clark Richards
