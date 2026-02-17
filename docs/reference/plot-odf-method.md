# Plot an odf Object

Plot data contained within an ODF object, using
[`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md)
to create panels of time-series plots for all the columns contained in
the `odf` object (or just those that contain at least one finite value,
if `blanks` is `FALSE`). If the object's `data` slot does not contain
`time`, then [`pairs()`](https://rdrr.io/r/graphics/pairs.html) is used
to plot all the elements in the `data` slot that contain at least one
finite value. These actions are both crude and there are no arguments to
control the behaviour, but this function is really just a stop-gap
measure, since in practical work
[odf](https://dankelley.github.io/oce/reference/odf-class.md) objects
are usually cast to other types, and those types tend to have more
useful plots.

## Usage

``` r
# S4 method for class 'odf'
plot(x, blanks = TRUE, debug = getOption("oceDebug"))
```

## Arguments

- x:

  an [odf](https://dankelley.github.io/oce/reference/odf-class.md)
  object.

- blanks:

  A logical value that indicates whether to include dummy plots for data
  items that lack any finite values.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## See also

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

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)

## Author

Dan Kelley
