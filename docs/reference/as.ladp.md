# Coerce Data Into an ladp object

This function assembles vectors of pressure and velocity, possibly also
with shears, salinity, temperature, etc.

## Usage

``` r
as.ladp(
  longitude,
  latitude,
  station,
  time,
  pressure,
  u,
  v,
  uz,
  vz,
  salinity,
  temperature,
  ...
)
```

## Arguments

- longitude:

  longitude in degrees east, or an `oce` object that contains the data
  otherwise given by `longitude` and the other arguments.

- latitude:

  latitude in degrees east (use negative in southern hemisphere).

- station:

  number or string indicating station ID.

- time:

  time at the start of the profile, constructed by e.g.
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

- pressure:

  pressure in decibars, through the water column.

- u:

  eastward velocity (m/s).

- v:

  northward velocity (m/s).

- uz:

  vertical derivative of eastward velocity (1/s).

- vz:

  vertical derivative of northward velocity (1/s).

- salinity:

  salinity through the water column, in practical salinity units.

- temperature:

  temperature through the water column.

- ...:

  optional additional data columns.

## Value

An [ladp](https://dankelley.github.io/oce/reference/ladp-class.md)
object.

## See also

Other things related to ladp data:
[`[[,ladp-method`](https://dankelley.github.io/oce/reference/sub-sub-ladp-method.md),
`[[<-,ladp-method`,
[`ladp-class`](https://dankelley.github.io/oce/reference/ladp-class.md),
[`plot,ladp-method`](https://dankelley.github.io/oce/reference/plot-ladp-method.md),
[`summary,ladp-method`](https://dankelley.github.io/oce/reference/summary-ladp-method.md)

## Author

Dan Kelley
