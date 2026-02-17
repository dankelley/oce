# Plot a landsat Object

Plot the data within a landsat image, or information computed from the
data. The second category includes possibilities such as an estimate of
surface temperature and the `"terralook"` estimate of a natural-color
view.

## Usage

``` r
# S4 method for class 'landsat'
plot(
  x,
  band,
  which = 1,
  decimate = TRUE,
  zlim,
  utm = FALSE,
  col = oce.colorsPalette,
  drawPalette = TRUE,
  showBandName = TRUE,
  alpha.f = 1,
  red.f = 1.7,
  green.f = 1.5,
  blue.f = 6,
  offset = c(0, -0.05, -0.2, 0),
  transform = diag(c(red.f, green.f, blue.f, alpha.f)),
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  a
  [landsat](https://dankelley.github.io/oce/reference/landsat-class.md)
  object.

- band:

  If given, the name of the band. For Landsat-8 data, this may be one
  of: `"aerosol"`, `"blue"`, `"green"`, `"red"`, `"nir"`, `"swir1"`,
  `"swir2"`, `"panchromatic"`, `"cirrus"`, `"tirs1"`, or `"tirs2"`. For
  Landsat-7 data, this may be one of `"blue"`, `"green"`, `"red"`,
  `"nir"`, `"swir1"`, `"tirs1"`, `"tirs2"`, `"swir2"`, or
  `"panchromatic"`. For Landsat data prior to Landsat-7, this may be one
  of `"blue"`, `"green"`, `"red"`, `"nir"`, `"swir1"`, `"tirs1"`,
  `"tirs2"`, or `"swir2"`. If `band` is not given, the (`"tirs1"`) will
  be used if it exists in the object data, or otherwise the first band
  will be used. In addition to the above, using `band="temperature"`
  will plot an estimate of at-satellite brightness temperature, computed
  from the `tirs1` band, and `band="terralook"` will plot a sort of
  natural color by combining the `red`, `green`, `blue` and `nir` bands
  according to the formula provided at
  `https://lta.cr.usgs.gov/terralook/what_is_terralook` (a website that
  worked once, but failed as of Feb 2, 2017).

- which:

  Desired plot type; 1=image, 2=histogram.

- decimate:

  An indication of the desired decimation, passed to
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) for
  image plots. The default yields faster plotting. Some decimation is
  sensible for full-size images, since no graphical displays can show 16
  thousand pixels on a side.

- zlim:

  Either a pair of numbers giving the limits for the colorscale, or
  `"histogram"` to have a flattened histogram (i.e. to maximally
  increase contrast throughout the domain.) If not given, the 1 and 99
  percent quantiles are calculated and used as limits.

- utm:

  A logical value indicating whether to use UTS (easting and northing)
  instead of longitude and latitude on plot.

- col:

  Either a function yielding colors, taking a single integer argument
  with the desired number of colors, or the string `"natural"`, which
  combines the information in the `red`, `green` and `blue` bands and
  produces a natural-hue image. In the latter case, the band designation
  is ignored, and the object must contain the three color bands.

- drawPalette:

  Indication of the type of palette to draw, if any. See
  [`imagep()`](https://dankelley.github.io/oce/reference/imagep.md) for
  details.

- showBandName:

  A logical indicating whether the band name is to plotted in the top
  margin, near the right-hand side.

- alpha.f:

  Argument used if `col="natural"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).

- red.f:

  Argument used if `col="natural"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).
  Higher values of `red.f` cause red hues to be emphasized (e.g. dry
  land).

- green.f:

  Argument used if `col="natural"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).
  Higher values of `green.f` emphasize green hues (e.g. forests).

- blue.f:

  Argument used if `band="terralook"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).
  Higher values of `blue.f` emphasize blue hues (e.g. ocean).

- offset:

  Argument used if `band="terralook"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).

- transform:

  Argument used if `band="terralook"`, to adjust colors with
  [`adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html).

- debug:

  Set to a positive value to get debugging information during
  processing.

- ...:

  optional arguments passed to plotting functions.

## Details

For Landsat-8 data, the `band` may be one of: `"aerosol"`, `"blue"`,
`"green"`, `"red"`, `"nir"`, `"swir1"`, `"swir2"`, `"panchromatic"`,
`"cirrus"`, `"tirs1"`, or `"tirs2"`.

For Landsat-7 data, `band` may be one of `"blue"`, `"green"`, `"red"`,
`"nir"`, `"swir1"`, `"tirs1"`, `"tirs2"`, `"swir2"`, or
`"panchromatic"`.

For Landsat data prior to Landsat-7, `band` may be one of `"blue"`,
`"green"`, `"red"`, `"nir"`, `"swir1"`, `"tirs1"`, `"tirs2"`, or
`"swir2"`.

If `band` is not given, the (`"tirs1"`) will be used if it exists in the
object data, or otherwise the first band will be used.

In addition to the above there are also some pseudo-bands that can be
plotted, as follows.

- Setting `band="temperature"` will plot an estimate of at-satellite
  brightness temperature, computed from the `tirs1` band.

- Setting `band="terralook"` will plot a sort of natural color by
  combining the `red`, `green`, `blue` and `nir` bands according to the
  formula provided at
  `https://lta.cr.usgs.gov/terralook/what_is_terralook` (a website that
  worked once, but failed as of Feb 2, 2017), namely that the `red`-band
  data are provided as the `red` argument of the
  [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) function, while the
  `green` argument is computed as 2/3 of the `green`-band data plus 1/3
  of the `nir`-band data, and the `blue` argument is computed as 2/3 of
  the `green`-band data minus 1/3 of the `nir`-band data. (This is not a
  typo: the `blue` band is not used.)

## See also

Other things related to landsat data:
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
`[[<-,landsat-method`,
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`landsat-class`](https://dankelley.github.io/oce/reference/landsat-class.md),
[`landsatAdd()`](https://dankelley.github.io/oce/reference/landsatAdd.md),
[`landsatTrim()`](https://dankelley.github.io/oce/reference/landsatTrim.md),
[`read.landsat()`](https://dankelley.github.io/oce/reference/read.landsat.md),
[`summary,landsat-method`](https://dankelley.github.io/oce/reference/summary-landsat-method.md)

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`plot,bremen-method`](https://dankelley.github.io/oce/reference/plot-bremen-method.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plot,gps-method`](https://dankelley.github.io/oce/reference/plot-gps-method.md),
[`plot,ladp-method`](https://dankelley.github.io/oce/reference/plot-ladp-method.md),
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
