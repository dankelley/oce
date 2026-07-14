# Plot an amsr Object

Plot an image of a component of an
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) object.

## Usage

``` r
# S4 method for class 'amsr'
plot(
  x,
  y,
  asp = NULL,
  breaks,
  col,
  colormap,
  zlim,
  zlab,
  pass = NULL,
  missingColor,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [amsr](https://dankelley.github.io/oce/reference/amsr-class.md)
  object.

- y:

  character value indicating the name of the band to plot; if not
  provided, `SST` (or a variant thereof) is used; see the documentation
  for the
  [amsr](https://dankelley.github.io/oce/reference/amsr-class.md) class
  for a list of bands.

- asp:

  optional numerical value giving the aspect ratio for plot. The default
  value, `NULL`, means to use an aspect ratio of 1 for world views, and
  a value computed from `ylim`, if the latter is specified in the `...`
  argument.

- breaks:

  optional numeric vector of the z values for breaks in the color
  scheme. If `colormap` is provided, it takes precedence over `breaks`
  and `col`.

- col:

  optional argument, either a vector of colors corresponding to the
  breaks, of length 1 less than the number of breaks, or a function
  specifying colors. If neither `col` or `colormap` is provided, then
  `col` defaults to
  [`oceColorsTemperature()`](https://dankelley.github.io/oce/reference/oceColorsTemperature.md).
  If `colormap` is provided, it takes precedence over `breaks` and
  `col`.

- colormap:

  a specification of the colormap to use, as created with
  [`colormap()`](https://dankelley.github.io/oce/reference/colormap.md).
  If `colormap` is NULL, which is the default, then a colormap is
  created to cover the range of data values, using
  [oceColorsTemperature](https://dankelley.github.io/oce/reference/oceColorsTemperature.md)
  color scheme. If `colormap` is provided, it takes precedence over
  `breaks` and `col`. See “Examples” for an example of using the "turbo"
  color scheme.

- zlim:

  optional numeric vector of length 2, giving the limits of the plotted
  quantity. A reasonable default is computed, if this is not given.

- zlab:

  optional character value that is shown in the top-right margin of the
  plot. If not given, this defaults to the name of the plotted variable.

- pass:

  either NULL (the default), or character value that is either
  `"ascending"` or `"descending"`. The value of `pass` is only examined
  for realtime data, which hold both ascending and descending passes in
  SST and related arrays.

- missingColor:

  optional list specifying colors to use for non-data categories. If not
  provided, a default is used. For type 1, that default is
  `list(land="papayaWhip", none="lightGray", bad="gray", rain="plum", ice="mediumVioletRed")`.
  For type 2, it is
  `list(coast="gray", land="papayaWhip", noObs="lightGray", seaIce="mediumVioletRed")`.
  Any colors may be used in place of these, but the names must match,
  and all names must be present.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  extra arguments passed to
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md),
  e.g. to control the view with `xlim` (for longitude) and `ylim` (for
  latitude).

## Details

In addition to fields named directly in the object, such as `SSTDay` and
`SSTNight`, it is also possible to plot computed fields, such as `SST`,
which combines the day and night fields.

## See also

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`plot,bremen-method`](https://dankelley.github.io/oce/reference/plot-bremen-method.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
[`plot,ladp-method`](https://dankelley.github.io/oce/reference/plot-ladp-method.md),
[`plot,landsat-method`](https://dankelley.github.io/oce/reference/plot-landsat-method.md),
[`plot,lisst-method`](https://dankelley.github.io/oce/reference/plot-lisst-method.md),
[`plot,lobo-method`](https://dankelley.github.io/oce/reference/plot-lobo-method.md),
[`plot,met-method`](https://dankelley.github.io/oce/reference/plot-met-method.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`plot,satellite-method`](https://dankelley.github.io/oce/reference/plot-satellite-method.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`plot,topo-method`](https://dankelley.github.io/oce/reference/plot-topo-method.md),
[`plot,windrose-method`](https://dankelley.github.io/oce/reference/plot-windrose-method.md),
[`plot,xbt-method`](https://dankelley.github.io/oce/reference/plot-xbt-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(coastlineWorld)
data(amsr) # see ?amsr for how to read and composite such objects

# Example 1: plot with default color scheme, oceColorsTemperature()
plot(amsr, "SST")
lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])


# Example 2: 'turbo' color scheme
plot(amsr, "SST", col = oceColorsTurbo)
lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])

```
