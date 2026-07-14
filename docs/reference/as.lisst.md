# Coerce Data Into a lisst Object

If `data` contains fewer than 42 columns, an error is reported. If it
contains more than 42 columns, only the first 42 are used. This is used
by
[`read.lisst()`](https://dankelley.github.io/oce/reference/read.lisst.md),
the documentation on which explains the meanings of the columns.

## Usage

``` r
as.lisst(
  data,
  filename = "",
  year = 0,
  tz = "UTC",
  longitude = NA,
  latitude = NA
)
```

## Arguments

- data:

  A table (or matrix) containing 42 columns, as in a LISST data file.

- filename:

  Name of file containing the data.

- year:

  Year in which the first observation was made. This is necessary
  because LISST timestamps do not indicate the year of observation. The
  default value is odd enough to remind users to include this argument.

- tz:

  Timezone of observations. This is necessary because LISST timestamps
  do not indicate the timezone.

- longitude:

  Longitude of observation.

- latitude:

  Latitude of observation.

## Value

A [lisst](https://dankelley.github.io/oce/reference/lisst-class.md)
object.

## See also

Other things related to lisst data:
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
`[[<-,lisst-method`,
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`plot,lisst-method`](https://dankelley.github.io/oce/reference/plot-lisst-method.md),
[`read.lisst()`](https://dankelley.github.io/oce/reference/read.lisst.md),
[`summary,lisst-method`](https://dankelley.github.io/oce/reference/summary-lisst-method.md)

## Author

Dan Kelley
