# Coerce Data Into a lobo Object

Coerce a dataset into a lobo dataset.

## Usage

``` r
as.lobo(
  time,
  u,
  v,
  salinity,
  temperature,
  pressure,
  nitrate,
  fluorescence,
  filename = ""
)
```

## Arguments

- time:

  vector of times of observation

- u:

  vector of x velocity component observations

- v:

  vector of y velocity component observations

- salinity:

  vector of salinity observations

- temperature:

  vector of temperature observations

- pressure:

  vector of pressure observations

- nitrate:

  vector of nitrate observations

- fluorescence:

  vector of fluorescence observations

- filename:

  source filename

## Value

A [lobo](https://dankelley.github.io/oce/reference/lobo-class.md)
object.

## See also

Other things related to lobo data:
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
`[[<-,lobo-method`,
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`read.lobo()`](https://dankelley.github.io/oce/reference/read.lobo.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`summary,lobo-method`](https://dankelley.github.io/oce/reference/summary-lobo-method.md)

## Author

Dan Kelley
