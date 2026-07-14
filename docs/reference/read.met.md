# Read a met File

Reads some meteorological file formats used by the Environment Canada
(see reference 1). Since the agency does not publish the data formats,
this function has to be adjusted every few years, when a user finds that
the format has changed. **Caution:** as of March 2022, this function
fails on some on Windows machines, for reasons that seem to be related
to the handling of both file encoding and system encoding. Adjusting the
`encoding` parameter of this function might help. If not, try reading
the data with [`read.csv()`](https://rdrr.io/r/utils/read.table.html)
and then using
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md) to
create a [met](https://dankelley.github.io/oce/reference/met-class.md)
object.

## Usage

``` r
read.met(
  file,
  type = NULL,
  skip = NULL,
  encoding = "latin1",
  tz = getOption("oceTz"),
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  a character string naming a file that holds met data.

- type:

  if `NULL`, which is the default, then an attempt is made to infer the
  type from the file contents. If this fails, it will be necessary for
  the user to provide a value for the `type` argument. The permitted
  choices are: (a) `"csv"` or `"csv1"` for an old CSV format no longer
  provided as of October 2019, (b) `"csv2"` for a CSV format noticed on
  the Environment Canada website in October 2019 (but note that the
  paired metadata file is ignored), (c) `"csv3"` for a CSV format
  noticed on the Environment Canada website in January 2020, and (d)
  `"xml2"` for an XML format that was noticed on the Environment Canada
  website in October 2019.

- skip:

  integer giving the number of header lines that precede the data. This
  is ignored unless `type` is `"csv"` or `"csv1"`, in which case a
  non-`NULL` value of `skip` is taken as the number of lines preceding
  the columnar data. Specifying `skip` is usually only needed if
  `read.met()` cannot find a line starting with `"Date/Time"` (or a
  similar string).

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- tz:

  timezone assumed for time data. This defaults to
  [getOption](https://rdrr.io/r/base/options.html)`("oceTz")`, which is
  very likely to be wrong. In a scientific context, where UTC is
  typically used for oceanographic measurement, it makes sense to set
  `tz="UTC"`. Note that these data files do not contain timezone
  information, instead giving data in Local Standard Time (LST). Since
  LST differs from city to city, users must make corrections to the
  time, as shown in the “Examples”, which use data for Halifax Nova
  Scotia, where LST is UTC-4.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## Value

A [met](https://dankelley.github.io/oce/reference/met-class.md) object.

## Sample of Usage


    # Example 1: "csv1" Environment Canada format (found to be obsolete as of Oct 2019)
    csv1 <- read.met(system.file("extdata", "test_met_vsn1.csv", package="oce"))
    csv1 <- oceSetData(csv1, "time", csv1[["time"]]+4*3600,
        note="add 4h to local time to get UTC time")

    # Example 2: "csv2" Environment Canada format (found to be obsolete as of Jan 2022)
    csv2 <- read.met(system.file("extdata", "test_met_vsn2.csv", package="oce"))
    csv2 <- oceSetData(csv2, "time", csv2[["time"]]+4*3600,
        note="add 4h to local time to get UTC time")

    # Example 3: "csv3" Environment Canada format. Note timezone correction
    csv3 <- read.met(system.file("extdata", "test_met_vsn3.csv", package="oce"))
    csv3 <- oceSetData(csv3, "time", csv3[["time"]]+4*3600,
        note="add 4h to local time to get UTC time")

    # Example 4: "xml2" format. (Uncertain timezone, so not corrected.)
    if (requireNamespace("XML", quietly=TRUE))
        xml2 <- read.met(system.file("extdata", "test_met_xml2.xml", package="oce"))

    # Example 5: download and plot
    library(oce)
    # Recreate data(met) and plot u(t) and v(t)
    metFile <- download.met(id=6358, year=2003, month=9, destdir=".")
    met <- read.met(metFile)
    met <- oceSetData(met, "time", met[["time"]]+4*3600,
        note="add 4h to local time to get UTC time")
    plot(met)

## References

1.  Environment Canada website for Historical Climate Data
    `https://climate.weather.gc.ca/index_e.html`

## See also

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)

## Author

Dan Kelley
