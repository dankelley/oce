# Create an xbt Object

Create an xbt Object

## Usage

``` r
as.xbt(
  z,
  temperature,
  longitude = NA,
  latitude = NA,
  filename = "",
  sequenceNumber = NA,
  serialNumber = ""
)
```

## Arguments

- z:

  numeric vector giving vertical coordinates of measurements. This is
  the negative of depth, i.e. `z` is 0 at the air-sea interface, and
  negative within the water column.

- temperature:

  numeric vector giving in-situ temperatures at the `z` values.

- longitude, latitude:

  location in degE and degN.

- filename:

  character value naming source file.

- sequenceNumber:

  numerical value of the sequence number of the XBT drop.

- serialNumber:

  character value holding the serial number of the XBT.

## Value

An [xbt](https://dankelley.github.io/oce/reference/xbt-class.md) object.

## See also

Other things related to xbt data:
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,xbt-method`,
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`read.xbt()`](https://dankelley.github.io/oce/reference/read.xbt.md),
[`read.xbt.noaa1()`](https://dankelley.github.io/oce/reference/read.xbt.noaa1.md),
[`read.xbt.noaa2()`](https://dankelley.github.io/oce/reference/read.xbt.noaa2.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md),
[`summary,xbt-method`](https://dankelley.github.io/oce/reference/summary-xbt-method.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md),
[`xbt.edf`](https://dankelley.github.io/oce/reference/xbt.edf.md),
[`xbt2.edf`](https://dankelley.github.io/oce/reference/xbt2.edf.md)

## Author

Dan Kelley
