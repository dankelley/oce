# Smooth a Section

Smooth a section, in any of several ways, working either in the vertical
direction or in both the vertical and lateral directions.

## Usage

``` r
sectionSmooth(
  section,
  method = "spline",
  x,
  xg,
  yg,
  xgl,
  ygl,
  xr,
  yr,
  df,
  gamma = 0.5,
  iterations = 2,
  trim = 0,
  pregrid = FALSE,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- section:

  A `section` object containing the section to be smoothed. For
  `method="spline"`, the pressure levels must match for each station in
  the section.

- method:

  A string or a function that specifies the method to use; see
  “Details”.

- x:

  Optional numerical vector, of the same length as the number of
  stations in `section`, which will be used in gridding in the lateral
  direction. If not provided, this defaults to
  [`geodDist`](https://dankelley.github.io/oce/reference/geodDist.md)`(section)`.

- xg, xgl:

  ignored in the `method="spline"` case, but passed to
  [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md)
  if `method="barnes"`, to kriging functions if `method="kriging"`, or
  to `method` itself, if it is a function. If `xg` is supplied, it
  defines the x component of the grid, which by default is the terms of
  station distances, x, along the track of the section. (Note that the
  grid `xg` is trimmed to the range of the data `x`, because otherwise
  it would be impossible to interpolate between stations to infer water
  depth, longitude, and latitude, which are all stored within the
  stations in the returned `section` object.) Alternatively, if `xgl` is
  supplied, the x grid is established using
  [`seq()`](https://rdrr.io/r/base/seq.html), to span the data with
  `xgl` elements. If neither of these is supplied, the output x grid
  will equal the input x grid.

- yg, ygl:

  similar to `xg` and `xgl`, but for pressure. (Note that trimming to
  the input `y` is not done, as it is for `xg` and `x`.) If `yg` is not
  given, it is determined from the deepest station in the section. If
  `ygl` was not given, then a grid is constructed to span the pressures
  of that deepest station with `ygl` elements. On the other hand, if
  `ygl` was not given, then the y grid will constructed from the
  pressure levels in the deepest station.

- xr, yr:

  influence ranges in x (along-section distance) and y (pressure),
  passed to
  [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md)
  if `method="barnes"` or to `method`, if the latter is a function. If
  missing, `xr` defaults to 1.5X the median inter-station distance and
  `yr` defaults to 0.2X the pressure range. Since these defaults have
  changed over the evolution of `sectionSmooth`, analysts ought to
  supply `xr` and `yr` in the function call, tailoring them to
  particular applications, and making the code more resistant to changes
  in `sectionSmooth`.

- df:

  Degree-of-freedom parameter, passed to
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html) if
  `method="spline"`, and ignored otherwise. If `df` is not provided, it
  defaults to 1/5-th of the number of stations containing non-`NA` data
  at the particular pressure level being processed, as `sectionSmooth`
  works its way through the water column.

- gamma, iterations, trim, pregrid:

  Values passed to
  [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md),
  if `method="barnes"`, and ignored otherwise. `gamma` is the factor by
  which `xr` and `yr` are reduced on each of succeeding iterations.
  `iterations` is the number of iterations to do. `trim` controls
  whether the gridded data are set to `NA` in regions with sparse data
  coverage. `pregrid` controls whether data are to be pre-gridded with
  [`binMean2D()`](https://dankelley.github.io/oce/reference/binMean2D.md)
  before being passed to
  [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md).

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  Optional extra arguments, passed to either
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html), if
  `method="spline"`, and ignored otherwise.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object of that has been smoothed in some way. Every data field that is
in even a single station of the input object is inserted into every
station in the returned value, and therefore the units represent all the
units in any of the stations, as do the flag names. However, the flags
are all set to `NA` values.

## Details

This function produces smoothed fields that might be useful in
simplifying graphical elements created with
[`plot,section-method()`](https://dankelley.github.io/oce/reference/plot-section-method.md).
As with any smoothing method, a careful analyst will compare the results
against the raw data, e.g. using
[`plot,section-method()`](https://dankelley.github.io/oce/reference/plot-section-method.md).
In addition the problem of falsely widening narrow features such as
fronts, there is also the potential to get unphysical results with spars
sampling near topographic features such as bottom slopes and ridges.

The `method` argument selects between three possible methods.

- For `method="spline"`, i.e. the default, the section is smoothed
  laterally, using
  [`smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html) on
  individual pressure levels. (If the pressure levels do not match up,
  [`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md)
  should be used first to create a `section` object that can be used
  here.) The `df` argument sets the degree of freedom of the spline,
  with larger values indicating less smoothing.

- For `method="barnes"`, smoothing is done across both horizontal and
  vertical coordinates, using
  [`interpBarnes()`](https://dankelley.github.io/oce/reference/interpBarnes.md).
  The output station locations are computed by linear interpolation of
  input locations, using
  [`approx()`](https://rdrr.io/r/stats/approxfun.html) on the original
  longitudes and longitudes of stations, with the independent variable
  being the distance along the track, computed with
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md).
  The values of `xg`, `yg`, `xgl` and `ygl` control the smoothing.

- For `method="kriging"`, an error is reported. This is because the code
  formerly used the
  [automap](https://CRAN.R-project.org/package=automap) package, but
  this was removed from CRAN in June 2025.

- If `method` is a function, then that function is applied to the
  (distance, pressure) data for each variable at a grid defined by `xg`,
  `xgl`, `yg` and `ygl`. The function must be of the form
  `function(x,y,z,xg,xr,yg,yr)`, and must return a matrix of the gridded
  result, with first index indicating the "grid" station number and
  second index indicating "grid" pressure. The `x` value that is
  supplied to this function is set as the distance along the section, as
  computed with
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md),
  and repeated for each of the points at each station. The corresponding
  pressures are provided in `y`, and the value to be gridded is in `z`,
  which may be `temperature` on one call to the function, `salinity` on
  another call, etc. The other quantities have the meanings as described
  below.

## See also

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md),
[`handleFlags,section-method`](https://dankelley.github.io/oce/reference/handleFlags-section-method.md),
[`initializeFlagScheme,section-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-section-method.md),
[`plot,section-method`](https://dankelley.github.io/oce/reference/plot-section-method.md),
[`read.section()`](https://dankelley.github.io/oce/reference/read.section.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
# Unsmoothed (Gulf Stream)
library(oce)
data(section)
gs <- subset(section, 115 <= stationId & stationId <= 125)
par(mfrow = c(2, 2))

plot(gs, which = "temperature")
mtext("Original data, without smoothing", line = 0.5)

# Spline
gsg <- sectionGrid(gs, p = seq(0, 5000, 100))
gsSpline <- sectionSmooth(gsg, "spline")
plot(gsSpline, which = "temperature")
mtext("sectionSmooth(..., method=\"spline\")", line = 0.5)

# Barnes
gsBarnes <- sectionSmooth(gs, "barnes", xr = 50, yr = 200)
plot(gsBarnes, which = "temperature")
mtext("sectionSmooth(..., method=\"barnes\")", line = 0.5)


```
