# Coerce Data Into a met Object

Coerces a dataset into a met dataset. This fills in only a few of the
typical data fields, so the returned object is much sparser than the
output from
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md).
Also, almost no metadata fields are filled in, so the resultant object
does not store station location, units of the data, data-quality flags,
etc. Anyone working with data from Environment Canada (reference 2) is
advised to use
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md)
instead of the present function.

## Usage

``` r
as.met(time, temperature, pressure, u, v, filename = "(constructed from data)")
```

## Arguments

- time:

  Either a vector of observation times (or character strings that can be
  coerced into times) or the output from `canadaHCD::hcd_hourly` (see
  reference 1).

- temperature:

  vector of temperatures.

- pressure:

  vector of pressures.

- u:

  vector of eastward wind speed in m/s.

- v:

  vector of northward wind speed in m/s.

- filename:

  optional string indicating data source

## Value

A [met](https://dankelley.github.io/oce/reference/met-class.md) object.

## References

1.  The `canadaHCD` package is in development by Gavin Simpson; see
    `https://github.com/gavinsimpson/canadaHCD` for instructions on how
    to download and install from GitHub.

2.  Environment Canada website for Historical Climate Data
    `https://climate.weather.gc.ca/index_e.html`

## See also

Other things related to met data:
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
`[[<-,met-method`,
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`read.met()`](https://dankelley.github.io/oce/reference/read.met.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`summary,met-method`](https://dankelley.github.io/oce/reference/summary-met-method.md)

## Author

Dan Kelley
