# Read a lisst File

Read a LISST data file. The file should contain 42 columns, with no
header. If there are fewer than 42 columns, an error results. If there
are more, only the first 42 are used. Note that
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md)
can recognize LISST files by their having a name ending in `".asc"` and
by having 42 elements on the first line. Even so, it is preferred to use
the present function, because it gives the opportunity to specify the
year and timezone, so that times can be calculated properly.

## Usage

``` r
read.lisst(
  file,
  year = 0,
  tz = "UTC",
  longitude = NA,
  latitude = NA,
  encoding = "latin1"
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- year:

  year in which the measurement of the series was made.

- tz:

  time zone.

- longitude:

  longitude of observation (stored in metadata)

- latitude:

  latitude of observation (stored in metadata)

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

## Value

x A [lisst](https://dankelley.github.io/oce/reference/lisst-class.md)
object.

## See also

Other things related to lisst data:
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
`[[<-,lisst-method`,
[`as.lisst()`](https://dankelley.github.io/oce/reference/as.lisst.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`plot,lisst-method`](https://dankelley.github.io/oce/reference/plot-lisst-method.md),
[`summary,lisst-method`](https://dankelley.github.io/oce/reference/summary-lisst-method.md)

## Author

Dan Kelley
