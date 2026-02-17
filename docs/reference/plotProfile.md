# Plot a ctd Profile

Plot a profile, showing variation of some quantity (or quantities) with
pressure, using the oceanographic convention of putting lower pressures
nearer the top of the plot. This works for any `oce` object that has a
pressure column in its `data` slot. The colors (`col.salinity`, etc.)
are only used if two profiles appear on a plot.

## Usage

``` r
plotProfile(
  x,
  xtype = "salinity+temperature",
  ytype = "pressure",
  eos = getOption("oceEOS", default = "gsw"),
  lty = 1,
  xlab = NULL,
  ylab = NULL,
  col = "black",
  col.salinity = "darkgreen",
  col.temperature = "red",
  col.rho = "blue",
  col.N2 = "brown",
  col.dpdt = "darkgreen",
  col.time = "darkgreen",
  pt.bg = "transparent",
  grid = TRUE,
  col.grid = "lightgray",
  lty.grid = "dotted",
  Slim,
  Clim,
  Tlim,
  densitylim,
  sigmalim,
  N2lim,
  Rrholim,
  dpdtlim,
  timelim,
  plim,
  xlim,
  ylim,
  lwd = par("lwd"),
  xaxs = "r",
  yaxs = "r",
  cex = 1,
  pch = 1,
  useSmoothScatter = FALSE,
  df,
  keepNA = FALSE,
  type = "l",
  mgp = getOption("oceMgp"),
  mar,
  add = FALSE,
  inset = FALSE,
  debug = getOption("oceDebug", 0),
  ...
)
```

## Arguments

- x:

  a [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
  object.

- xtype:

  item(s) to be plotted on the x axis, either a character value taken
  from the following list, or a numeric vector of length matching the
  `pressure` field stored in `x`. (In the second case, as of version
  1.7-11, a label is auto-constructed, unless the user supplied a
  character value for `xlab`.)

  - `"salinity"` Profile of salinity.

  - `"conductivity"` Profile of conductivity.

  - `"temperature"` Profile of *in-situ* temperature.

  - `"theta"` Profile of potential temperature.

  - `"density"` Profile of density (expressed as \\\sigma\_\theta\\).

  - `"index"` Index of sample (useful for working with
    [`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md)).

  - `"salinity+temperature"` Profile of salinity and temperature within
    a single axis frame.

  - `"N2"` Profile of square of buoyancy frequency \\N^2\\, calculated
    with [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md)
    with an optional argument setting of `df=length(x[["pressure"]])/4`
    to do some smoothing.

  - `"density+N2"` Profile of sigma0 and the square of buoyancy
    frequency within a single axis frame.

  - `"density+dpdt"` Profile of sigma0 and dP/dt for the sensor. The
    latter is useful in indicating problems with the deployment. It is
    calculated by first differencing pressure and then using a smoothing
    spline on the result (to avoid grid-point wiggles that result
    because the SBE software only writes 3 decimal places in pressure).
    Note that dP/dt may be off by a scale factor; this should not be a
    problem if there is a `time` column in the `data` slot, or a
    `sample.rate` in the `metadata` slot.

  - `"sigma0"`, `"sigma1"`, `"sigma2"`, `"sigma3"` and `"sigma4"`
    Profile of potential density referenced to 0dbar (i.e. the surface),
    1000dbar, 2000dbar, 3000dbar, and 4000dbar.

  - `"spice"`, `"spiciness0"` `"spiciness1"` or `"spiciness2"` Profile
    of named quantity. For `spice`,
    [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
    is called with the `eos` argument set to `"unesco"`. Otherwise,
    [`gsw::gsw_spiciness0()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness0.md)',
    [`gsw::gsw_spiciness1()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness1.md)'
    or
    [`gsw::gsw_spiciness2()`](http://teos-10.github.io/GSW-R/reference/gsw_spiciness2.md)'
    is called.

  - `"Rrho"` Profile of Rrho, defined in the diffusive sense.

  - `"RrhoSF"` Profile of Rrho, defined in the salt-finger sense.

- ytype:

  variable to use on y axis. The valid choices are: `"pressure"` (the
  default), `"z"`, `"depth"`, `"sigmaTheta"` and `"sigma0"`.

- eos:

  equation of state to be used, either `"unesco"` or `"gsw"`.

- lty:

  line type for the profile.

- xlab:

  optional label for x axis (at top of plot). If not provided, a label
  is inferred from the value of `xtype`. For the user-supplied case,
  bear in mind that the easy way to get units is to use an expression,
  e.g. `xlab=expression("Acceleration ["*m/s^2*"]")`.

- ylab:

  optional label for y axis. See `xlab` for a note on units. Setting
  `ylab=""` prevents labelling the axis.

- col:

  color for a general profile.

- col.salinity:

  color for salinity profile (see “Details”).

- col.temperature:

  color for temperature (see “Details”).

- col.rho:

  color for density (see “Details”).

- col.N2:

  color for square of buoyancy frequency (see “Details”).

- col.dpdt:

  color for dP/dt.

- col.time:

  color for delta-time.

- pt.bg:

  inside color for symbols with `pch` in 21:25

- grid:

  logical, set to `TRUE` to get a grid.

- col.grid:

  color for grid.

- lty.grid:

  line type for grid.

- Slim:

  optional limit for the salinity axis, which can either represent
  Practical Salinity or Absolute Salinity.

- Clim:

  optional limit for the conductivity axis.

- Tlim:

  optional limit for the temperature axis, which can represent in-situ
  temperature, potential temperature, or Conservative Temperature.

- densitylim:

  optional limit for density axis.

- sigmalim:

  optional limit for the density-anomaly axis, which can represent
  `sigmaTheta`, `sigma0`, `sigma1`, `sigma2`, `sigma3` or `sigma4`.

- N2lim:

  optional limit for the N2 axis.

- Rrholim:

  optional limit for the density ratio axis.

- dpdtlim:

  optional limit for the dp/dt axis.

- timelim:

  optional limit for the delta-time axis.

- plim:

  optional limit for the pressure axis, ignored unless
  `ytype=="pressure"`, in which case it takes precedence over `ylim`.

- xlim:

  optional limit for x axis, which can apply to any plot type. This is
  ignored if the plotted x variable is something for which a limit may
  be specified with an argument, e.g. `xlim` is ignored for a salinity
  profile, because `Slim` ought to be given in such a case.

- ylim:

  optional limit for y axis, which can apply to any plot type, although
  is overridden by `plim` if `ytype` is `"pressure"` or by `densitylim`
  if `ytype` is `"sigmaTheta"`.

- lwd:

  line width value for data line

- xaxs:

  value of [`par()`](https://rdrr.io/r/graphics/par.html) `xaxs` to use

- yaxs:

  value of [`par()`](https://rdrr.io/r/graphics/par.html) `yaxs` to use

- cex:

  size to be used for plot symbols (see
  [`par()`](https://rdrr.io/r/graphics/par.html))

- pch:

  code for plotting symbol (see
  [`par()`](https://rdrr.io/r/graphics/par.html)).

- useSmoothScatter:

  boolean, set to `TRUE` to use
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)
  instead of [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to
  draw the plot.

- df:

  optional argument, passed to
  [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md) if
  provided, and if a plot using \\N^2\\ is requested.

- keepNA:

  FALSE

- type:

  type of plot to draw, using the same scheme as
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- mgp:

  3-element numerical vector to use for
  [par](https://rdrr.io/r/graphics/par.html)`(mgp)`, and also for
  [par](https://rdrr.io/r/graphics/par.html)`(mar)`, computed from this.
  The default is tighter than the R default, in order to use more space
  for the data and less for the axes.

- mar:

  Four-element numerical value to be used to set the plot margins, with
  a call to [par](https://rdrr.io/r/graphics/par.html)`(mar)` prior to
  the plot. If this is not supplied, a reasonable default will be set
  up.

- add:

  A logical value that controls whether to add to an existing plot. (It
  makes sense to use `add=TRUE` in the `panel` argument of a
  [`coplot()`](https://rdrr.io/r/graphics/coplot.html), for example.)

- inset:

  A logical value indicating whether to draw an inset plot. Setting this
  to `TRUE` will prevent the present function from adjusting the
  margins, which is necessary because margin adjustment is the basis for
  the method used by
  [`plotInset()`](https://dankelley.github.io/oce/reference/plotInset.md).

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  optional arguments passed to other functions. A common example is to
  set `df`, for use in
  [`swN2()`](https://dankelley.github.io/oce/reference/swN2.md)
  calculations.

## Value

None.

## See also

[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
scans ctd information from a file,
[`plot,ctd-method()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md)
is a general plotting function for `ctd` objects, and
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md) plots
a temperature-salinity diagrams.

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
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md)

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
[`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`ctdRepair()`](https://dankelley.github.io/oce/reference/ctdRepair.md),
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md),
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md),
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.odv()`](https://dankelley.github.io/oce/reference/read.ctd.odv.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(ctd)
plotProfile(ctd, xtype = "temperature")

```
