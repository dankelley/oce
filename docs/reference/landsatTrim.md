# Trim a landsat Image to a Geographical Region

Trim a landsat image to a latitude-longitude box. This is only an
approximate operation, because landsat images are provided in x-y
coordinates, not longitude-latitude coordinates.

## Usage

``` r
landsatTrim(x, ll, ur, box, debug = getOption("oceDebug"))
```

## Arguments

- x:

  a
  [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
  object.

- ll:

  A list containing `longitude` and `latitude`, for the lower-left
  corner of the portion of the image to retain, or a vector with first
  element longitude and second element latitude. If provided, then `ur`
  must also be provided, but `box` cannot.

- ur:

  A list containing `longitude` and `latitude`, for the upper-right
  corner of the portion of the image to retain, or a vector with first
  element longitude and second element latitude. If provided, then `ll`
  must also be provided, but `box` cannot.

- box:

  A list containing `x` and `y` (each of length 2), corresponding to the
  values for `ll` and `ur`, such as would be produced by a call to
  `locator(2)`. If provided, neither `ll` nor `ur` may be provided.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or a higher value for more debugging.

## Value

A [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
object, with data having been trimmed as specified.

## Details

As of June 25, 2015, the matrices storing the image data are trimmed to
indices determined by linear interpolation based on the location of the
`ll` and `ur` corners within the lon-lat corners specified in the image
data. (A previous version trimmed in UTM space, and in fact this may be
done in future also, if a problem in lonlat/utm conversion is resolved.)
An error results if there is no intersection between the trimming box
and the image box.

## See also

The documentation for the
[landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
class explains the structure of landsat objects, and also outlines the
other functions dealing with them.

Other things related to landsat data:
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
`[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

## Author

Dan Kelley and Clark Richards
