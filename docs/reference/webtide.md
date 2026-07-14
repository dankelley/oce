# Get a Tidal Prediction From a WebTide Database

Get a tidal prediction from a WebTide database. This only works if the
standalone WebTide application is installed, and if it is installed in a
standard location. The details of installation are not within the oce
purview.

## Usage

``` r
webtide(
  action = c("map", "predict"),
  longitude,
  latitude,
  node,
  time,
  basedir = getOption("webtide"),
  region = "nwatl",
  plot = TRUE,
  tformat,
  pch = 20,
  cex = 0.5,
  nodecol = "black",
  landcol = rgb(0.82, 0.71, 0.55, 0.5),
  debug = getOption("oceDebug"),
  ...
)
```

## Source

The WebTide software may be downloaded for free at the Department of
Fisheries and Oceans (Canada) website at
`http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php`
(checked February 2016 and May 2017).

## Arguments

- action:

  An indication of the action, either `action="map"` to draw a map or
  `action="predict"` to get a prediction; see “Details”.

- longitude, latitude:

  optional location at which prediction is required (ignored if `node`
  is given).

- node:

  optional integer relating to a node in the database. If `node` is
  given, then neither `latitude` nor `longitude` may be given. If `node`
  is positive, then specifies indicates the node. If it is negative,
  [`locator()`](https://rdrr.io/r/graphics/locator.html) is called so
  that the user can click (once) on the map, after which the node is
  displayed on the map.

- time:

  a vector of times (in the UTC timezone) at which prediction is to be
  made. If not supplied, this will be the week starting at the present
  time, computed with
  [`presentTime()`](https://dankelley.github.io/oce/reference/presentTime.md),
  with a 15 minute increment.

- basedir:

  directory containing the `WebTide` application. For example, the
  author uses `"/usr/local/WebTide"` for this value, because that is
  where he installed the webtide materials, e.g.
  "/usr/local/WebTide/data/nwatl"\` is where his North Atlantic data
  files reside.

- region:

  database region, given as a directory name in the WebTide directory.
  For example, `h3o` is for Halifax Harbour, `nwatl` is for the
  northwest Atlantic, and `sshelf` is for the Scotian Shelf and Gulf of
  Maine.

- plot:

  boolean indicating whether to plot.

- tformat:

  optional argument passed to
  [`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md),
  for plot types that call that function. (See
  [`strptime()`](https://rdrr.io/r/base/strptime.html) for the format
  used.)

- pch:

  integer giving the character code (default is 20, for a bullet), used
  if a map-style plot is requested.

- cex:

  numeric giving the character expansion factor (default is 0.5), used
  if a map-style plot is requested.

- nodecol:

  colour to be used for dots on a map-style plot.

- landcol:

  colour of land, used in plotting maps. The default is a
  semi-transparent tan colour. Set to `"transparent"` to NULL to skip
  the drawing of land.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- ...:

  optional arguments passed to plotting functions. A common example is
  to set `xlim` and `ylim`, to focus a map region.

## Value

The value depends on `action`:

- If `action="map"` the return value is an indication of the location of
  a selected node, or (if `node` is NULL) of all nodes.

- If `action="predict"`, the return value is a list containing a vector
  of times (`time`), as well as vectors of the predicted `elevation` in
  metres and the predicted horizontal components of velocity, `u` and
  `v`, along with the `node` number, and the `basedir` and `region` as
  supplied to this function. If `plot` is `FALSE`, this value is
  returned invisibly.

## Details

There are two methods of using this function. *Case 1:* `action="map"`.
In this case, if `plot` is `FALSE`, a data frame is returned, containing
all the `node`s in the selected database, along with all the `latitude`s
and `longitude`s. This value is also returned (silently) if `plot` is
true, but in that case, a plot is drawn to indicate the node locations.
If `latitude` and `longitude` are given, then the node nearest that spot
is indicated on the map; otherwise, if `node` is given, then the
location of that node is indicated. There is also a special case: if
`node` is negative and
[`interactive()`](https://rdrr.io/r/base/interactive.html) is `TRUE`,
then [`locator()`](https://rdrr.io/r/graphics/locator.html) is called,
and the node nearest the spot where the user clicks the mouse is
indicated in the plot and in the return value.

*Case 2:* `action="predict"`. If `plot` is `FALSE`, then a list is
returned, indicating `time`, predicted `elevation`, velocity components
`u` and `v`, `node` number, the name of the `basedir`, and the `region`.
If `plot` is `TRUE`, this list is returned silently, and either a map or
a set of three time-series plots (for u, v and water level) is plotted.
(In the second case, users may wish to call `par(mfrow=c(3,1))` first.)

## Caution

WebTide is not an open-source application, so the present function was
designed based on little more than guesses about the WebTide file
structure. Users should be on the lookout for odd results.

## Sample of Usage


    # needs WebTide at the system level
    library(oce)
    # 1. prediction at Halifax NS
    longitude <- -63.57
    latitude <- 44.65
    prediction <- webtide("predict", longitude=longitude, latitude=latitude)
    mtext(paste0("prediction at ", latitude, "N and ", longitude, "E"), line=0.75, side=3)
    # 2. map
    webtide(lon=-63.57,lat=44.65,xlim=c(-64,-63),ylim=c(43.0,46))

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md)

## Author

Dan Kelley
