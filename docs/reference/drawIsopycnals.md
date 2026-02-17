# Add Isopycnal Curves to a TS Plot

Adds isopycnal lines to an existing temperature-salinity plot. This is
called by
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md), and
may be called by the user also, e.g. if an image plot is used to show TS
data density.

## Usage

``` r
drawIsopycnals(
  nlevels = 6,
  levels,
  rotate = TRUE,
  rho1000 = FALSE,
  digits = 2,
  eos = getOption("oceEOS", default = "gsw"),
  longitude = NULL,
  latitude = NULL,
  trimIsopycnals = TRUE,
  gridIsopycnals = c(50, 50),
  cex = 0.75 * par("cex"),
  col = "darkgray",
  lwd = par("lwd"),
  lty = par("lty"),
  debug = getOption("oceDebug")
)
```

## Arguments

- nlevels:

  suggested number of density levels (i.e. isopycnal curves); ignored if
  `levels` is supplied. If this is set to 0, no isopycnal are drawn (see
  also `levels`, next).

- levels:

  optional density levels to draw. If this is `NULL`, then no isopycnals
  are drawn.

- rotate:

  boolean, set to `TRUE` to write all density labels horizontally.

- rho1000:

  boolean, set to `TRUE` to write isopycnal labels as e.g. 1024 instead
  of 24.

- digits:

  minimum number of decimal digits to use in label (supplied to
  [`round()`](https://rdrr.io/r/base/Round.html)). If the density range
  is very small, `drawIsopycnals()` will increase value of `digits`, to
  try to make labels be distinct.

- eos:

  equation of state to be used, either `"unesco"` or `"gsw"`. If it is
  `"gsw"` then `latitude` and `longitude` must be supplied, since these
  are needed to computer density in that formulation.

- longitude, latitude:

  numerical values giving the location to be used in density
  calculations, if `eos` is `"gsw"`.

- trimIsopycnals:

  logical value (`TRUE` by default) that indicates whether to trim
  isopycnal curves (if drawn) to the region of temperature-salinity
  space for which density computations are considered to be valid in the
  context of the chosen `eos`; see the “Details” of the documentation
  for [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md).

- gridIsopycnals:

  a parameter that controls how the isopycnals are computed. This may be
  NULL, or an integer vector of length 2. *Case 1:* if `gridIsopycnals`
  is NULL, then the isopycnals are drawn by tracing density isopleths in
  salinity-temperature space. This method was used as the default prior
  to version 1.7-11, but it was found to yield staircase-like isopycnal
  curves for highly zoomed-in plots (e.g. with millidegree temperature
  ranges). *Case 2 (the new default):* If `gridIsopycnals` is a
  two-element integer vector, then a grid of density is constructed,
  with `gridIsopycnals[1]` salinity levels and `gridIsopycnals[2]`
  temperature levels, and then
  [`contourLines()`](https://rdrr.io/r/grDevices/contourLines.html) is
  used to trace the isopycnals. The default value of `gridIsopycnals`
  yields a grid of millimeter-scale spacing for a typical plot.

- cex:

  size for labels.

- col:

  color for lines and labels.

- lwd:

  line width for isopycnal curves

- lty:

  line type for isopycnal curves

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

None.

## Details

The default method of drawing isopycnals was changed in February of
2023, so that even plots that are zoomed in to have millidegree
temperature ranges will have smooth curves. See the discussion of
`gridIsopycnals` for details.

## References

- Fofonoff, N. P., and R. C. Millard. "Algorithms for Computation of
  Fundamental Properties of Seawater." UNESCO Technical Papers in Marine
  Research. SCOR working group on Evaluation of CTD data;
  UNESCO/ICES/SCOR/IAPSO Joint Panel on Oceanographic Tables and
  Standards, 1983. `https://unesdoc.unesco.org/ark:/48223/pf0000059832`.

- McDougall, Trevor J., David R. Jackett, Daniel G. Wright, and Rainer
  Feistel. "Accurate and Computationally Efficient Algorithms for
  Potential Temperature and Density of Seawater." Journal of Atmospheric
  and Oceanic Technology 20, no. 5 (May 1, 2003): 730-41.
  `https://journals.ametsoc.org/jtech/article/20/5/730/2543/Accurate-and-Computationally-Efficient-Algorithms`.

## See also

[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md), which
calls this.

## Author

Dan Kelley
