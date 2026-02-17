# Download and Cache a met File

`download.met()` attempts to download data from Environment Canada's
historical-data website, and to cache the files locally. Lacking a
published API, this function must rely on reverse-engineering of queries
handled by that web server. For that reason, it is brittle.

## Usage

``` r
download.met(
  id,
  year,
  month,
  deltat,
  type = "xml",
  destdir = ".",
  destfile,
  force = FALSE,
  quiet = FALSE,
  debug = getOption("oceDebug")
)
```

## Arguments

- id:

  A number giving the "Station ID" of the station of interest. If not
  provided, `id` defaults to 6358, for Halifax International Airport.
  See “Details”.

- year:

  A number giving the year of interest. Ignored unless `deltat` is
  `"hour"`. If `year` is not given, it defaults to the present year.

- month:

  A number giving the month of interest. Ignored unless `deltat` is
  `"hour"`. If `month` is not given, it defaults to the present month.

- deltat:

  Optional character string indicating the time step of the desired
  dataset. This may be `"hour"` or `"month"`. If `deltat` is not given,
  it defaults to `"hour"`.

- type:

  String indicating which type of file to download, either `"xml"` (the
  default) for an XML file or `"csv"` for a CSV file.

- destdir:

  Optional string indicating the directory in which to store downloaded
  files. If not supplied, `"."` is used, i.e. the data file is stored in
  the present working directory.

- destfile:

  Optional string indicating the name of the file. If not supplied, the
  file name is constructed from the other parameters of the function
  call, so subsequent calls with the same parameters will yield the same
  result, thus providing the key to the caching scheme.

- force:

  Logical value indicating whether to force a download, even if the file
  already exists locally.

- quiet:

  Logical value passed to
  [`download.file()`](https://rdrr.io/r/utils/download.file.html); a
  `TRUE` value silences output.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

String indicating the full pathname to the downloaded file.

## Details

If this function fails, users might try using Gavin Simpson's
`canadaHCD` package (reference 2). This package maintains a copy of the
Environment Canada listing of stations, and its `find_station()`
function provides an easy way to determine Station IDs. After that, its
`hcd_hourly` function (and related functions) make it easy to read data.
These data can then be converted to the `met` class with
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
although doing so leaves many important metadata blank.

## Sample of Usage

    library(oce)
    # Download data for Halifax International Airport, in September
    # of 2003. This dataset is used for data(met) provided with oce.
    # Note that requests for data after 2012 month 10 yield all
    # missing values, for reasons unknown to the author.
    metFile <- download.met(6358, 2003, 9, destdir=".")
    met <- read.met(metFile)

## References

1.  Environment Canada website for Historical Climate Data
    `https://climate.weather.gc.ca/index_e.html`

2.  Gavin Simpson's `canadaHCD` package on GitHub
    `https://github.com/gavinsimpson/canadaHCD`

## See also

The work is done with
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

Other functions that download files:
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`as.met()`](https://dankelley.github.io/oce/reference/as.met.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)

## Author

Dan Kelley
