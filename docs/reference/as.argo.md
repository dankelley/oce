# Coerce Data Into an argo Object

Coerce a dataset into an argo dataset. This is not the right way to read
official argo datasets, which are provided in NetCDF format and may be
read with
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md).

## Usage

``` r
as.argo(
  time,
  longitude,
  latitude,
  salinity,
  temperature,
  pressure,
  units = NULL,
  id,
  filename = "",
  missingValue
)
```

## Arguments

- time:

  a vector of POSIXct times.

- longitude:

  a vector of longitudes.

- latitude:

  a vector of latitudes.

- salinity:

  a vector of salinities.

- temperature:

  a vector of temperatures.

- pressure:

  a vector of pressures.

- units:

  an optional list containing units. If `NULL`, the default, then
  `"degree east"` is used for `longitude`, `"degree north"` for
  `latitude`, `""` for `salinity`, `"ITS-90"` for `temperature`, and
  `"dbar"` for `pressure`.

- id:

  an identifier for the argo float, typically a number, but stored
  within the object in a character form. (For example, the dataset
  retrieved with `data(argo)` has an `id` of `"6900388"`.)

- filename:

  a source filename, which defaults to an empty string.

- missingValue:

  an optional missing value, indicating data values that should be taken
  as `NA`.

## Value

An [argo](https://dankelley.github.io/oce/reference/argo-class.md)
object.

## See also

The documentation for the
[argo](https://dankelley.github.io/oce/reference/argo-class.md) class
explains the structure of argo objects, and also outlines the other
functions dealing with them.

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley
