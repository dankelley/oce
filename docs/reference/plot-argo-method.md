# Plot an argo Object

Plot a summary diagram for argo data.

## Usage

``` r
# S4 method for class 'argo'
plot(
  x,
  which = 1,
  level,
  coastline = c("best", "coastlineWorld", "coastlineWorldMedium", "coastlineWorldFine",
    "none"),
  cex = 1,
  pch = 1,
  type = "p",
  col = 1,
  fill = FALSE,
  projection = NULL,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  tformat,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  object.

- which:

  list of desired plot types, one of the following. Note that
  [`oce.pmatch()`](https://dankelley.github.io/oce/reference/ocePmatch.md)
  is used to try to complete partial character matches, and that an
  error will occur if the match is not complete (e.g. `"salinity"`
  matches to both `"salinity ts"` and `"salinity profile"`.).

  - `which=1`, `which="trajectory"` or `which="map"` gives a plot of the
    argo trajectory, with the coastline, if one is provided.

  - `which=2` or `"salinity ts"` gives a time series of salinity at the
    indicated level(s)

  - `which=3` or `"temperature ts"` gives a time series of temperature
    at the indicated level(s)

  - `which=4` or `"TS"` gives a TS diagram at the indicated level(s)

  - `which=5` or `"salinity profile"` gives a salinity profile

  - `which=6` or `"temperature profile"` gives a temperature profile

  - `which=7` or `"sigma0 profile"` gives a sigma0 profile

  - `which=8` or `"spice profile"` gives a spiciness profile, referenced
    to the surface. (This is the same as using `which=9`.)

  - `which=9` or `"spiciness0 profile"` gives a profile of spiciness
    referenced to a pressure of 0 dbar, i.e. the surface. (This is the
    same as using `which=8`.)

  - `which=10` or `"spiciness1 profile"` gives a profile of spiciness
    referenced to a pressure of 1000 dbar.

  - `which=11` or `"spiciness2 profile"` gives a profile of spiciness
    referenced to a pressure of 2000 dbar.

- level:

  depth pseudo-level to plot, for `which=2` and higher. May be an
  integer, in which case it refers to an index of depth (1 being the
  top) or it may be the string "all" which means to plot all data.

- coastline:

  character string giving the coastline to be used in an Argo-location
  map, or `"best"` to pick the one with highest resolution, or `"none"`
  to avoid drawing the coastline.

- cex:

  size of plotting symbols to be used if `type="p"`.

- pch:

  type of plotting symbols to be used if `type="p"`.

- type:

  plot type, either `"l"` or `"p"`.

- col:

  optional list of colors for plotting.

- fill:

  either a logical, indicating whether to fill the land with light-gray,
  or a color name. Owing to problems with some projections, the default
  is not to fill.

- projection:

  character value indicating the projection to be used in trajectory
  maps. If this is `NULL`, no projection is used, although the plot
  aspect ratio will be set to yield zero shape distortion at the mean
  float latitude. If `projection="automatic"`, then one of two
  projections is used: stereopolar (i.e. `"+proj=stere +lon_0=X"` where
  `X` is the mean longitude), or Mercator (i.e. `"+proj=merc"`)
  otherwise. Otherwise, `projection` must be a character string
  specifying a projection in the notation used by
  [`oceProject()`](https://dankelley.github.io/oce/reference/oceProject.md)
  and
  [`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md).

- mgp:

  a 3-element numerical vector to use for `par(mgp)`, and also for
  `par(mar)`, computed from this. The default is tighter than the R
  default, in order to use more space for the data and less for the
  axes.

- mar:

  value to be used with `par("mar")`.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- debug:

  debugging flag.

- ...:

  optional arguments passed to plotting functions.

## Value

None.

## See also

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
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
data(argo)
tc <- cut(argo[["time"]], "year")
# Example 1: plot map, which reveals float trajectory.
plot(argo, pch = as.integer(tc))
year <- substr(levels(tc), 1, 4)
data(topoWorld)
contour(topoWorld[["longitude"]], topoWorld[["latitude"]],
    topoWorld[["z"]],
    add = TRUE
)
legend("bottomleft", pch = seq_along(year), legend = year, bg = "white", cex = 3 / 4)


# Example 2: plot map, TS, T(z) and S(z). Note the use
# of handleFlags(), to skip over questionable data.
plot(handleFlags(argo), which = c(1, 4, 6, 5))

```
