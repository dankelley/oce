# Read a gps File

Reads GPX format files by simply finding all longitudes and latitudes;
in other words, information on separate tracks, or waypoints, etc., is
lost.

## Usage

``` r
read.gps(
  file,
  type = NULL,
  encoding = "latin1",
  debug = getOption("oceDebug"),
  processingLog
)
```

## Arguments

- file:

  name of file containing gps data.

- type:

  type of file, which will be inferred from examination of the data if
  not supplied. In the present version, the only choice for `type` is
  `"gpx"`.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  set to TRUE to print information about the header, etc.

- processingLog:

  ignored.

## Value

A [gps](https://dankelley.github.io/oce/reference/gps-class.md) object.

## See also

Other things related to gps data:
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
`[[<-,gps-method`,
[`as.gps()`](https://dankelley.github.io/oce/reference/as.gps.md),
[`gps-class`](https://dankelley.github.io/oce/reference/gps-class.md),
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
[`summary,gps-method`](https://dankelley.github.io/oce/reference/summary-gps-method.md)

## Author

Dan Kelley
