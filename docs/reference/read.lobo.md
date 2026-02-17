# Read a lobo File

Read a data file created by a LOBO instrument.

## Usage

``` r
read.lobo(file, cols = 7, encoding = "latin1", processingLog)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- cols:

  number of columns in dataset.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- processingLog:

  if provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

## Value

A [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
object.

## Details

This version of `read.lobo` is really quite crude, having been developed
mainly for a “predict the Spring bloom” contest at Dalhousie University.
In particular, the function assumes that the data columns are exactly as
specified in the Examples section; if you reorder the columns or add new
ones, this function is unlikely to work correctly. Furthermore, it
should be noted that the file format was inferred simply by downloading
files; the supplier makes no claims that the format will be fixed in
time. It is also worth noting that there is no
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md)
equivalent to `read.lobo`, because the file format has no recognizable
header.

## Sample of Usage

    library(oce)
    uri <- paste("http://lobo.satlantic.com/cgi-bin/nph-data.cgi?",
        "min_date=20070220&max_date=20070305",
        "&x=date&",
        "y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
        "data_format=text", sep="")
    lobo <- read.lobo(uri)

## See also

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`as.lobo()`](https://dankelley.github.io/oce/reference/as.lobo.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`summary,lobo-method`](https://dankelley.github.io/oce/reference/summary-lobo-method.md)

## Author

Dan Kelley
