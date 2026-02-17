# Read a section File

Read a file that contains a series of `ctd` profiles that make up an
oceanographic section. Only *exchange BOT* comma-separated value format
is permitted at this time, but other formats may be added later. It
should also be noted that the parsing scheme was developed after
inspection of the A03 data set (see Examples). This may cause problems
if the format is not universal. For example, the header must name the
salinity column "`CTDSAL`"; if not, salinity values will not be read
from the file.

## Usage

``` r
read.section(
  file,
  directory,
  sectionId = "",
  flags,
  ship = "",
  scientist = "",
  institute = "",
  missingValue = -999,
  encoding = "latin1",
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  A file containing a set of CTD observations. At present, only the
  *exchange BOT* format is accepted (see “Details”).

- directory:

  A character string indicating the name of a directory that contains a
  set of CTD files that hold individual stations in the section.

- sectionId:

  Optional string indicating the name for the section. If not provided,
  the section ID is determined by examination of the file header.

- flags:

  Ignored, and deprecated (will be disallowed in a future version).

- ship:

  Name of the ship carrying out the sampling.

- scientist:

  Name of chief scientist aboard ship.

- institute:

  Name of chief scientist's institute.

- missingValue:

  Numerical value used to indicate missing data.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- processingLog:

  If provided, the action item to be stored in the log. This is
  typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object.

## Disambiguating salinity

WOCE datasets commonly have a column named `CTDSAL` for salinity
inferred from a CTD and `SALNTY` (not a typo) for salinity derived from
bottle data. If only one of these is present in the data file, the data
will be called `salinity` in the `data` slot of the return value.
However, if both are present, then `CTDSAL` is stored as `salinity` and
`SALNTY` is stored as `salinityBottle`.

## References

Several repository sites provide section data. A reasonably stable
example is `https://cchdo.ucsd.edu`, but a search on
`"WOCE bottle data"` should turn up other sites, if this ceases to
exist. Only the so-called *exchange BOT* data format can be processed by
`read.section()` at this time. Data names are inferred from column
headings using
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md).

## See also

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley
