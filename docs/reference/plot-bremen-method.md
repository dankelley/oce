# Plot a bremen Object

Plot a
[bremen](https://dankelley.github.io/oce/reference/bremen-class.md)
object. If the first argument seems to be a CTD dataset, this uses
[`plot,ctd-method()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md);
otherwise, that argument is assumed to be a
[ladp](https://dankelley.github.io/oce/reference/ladp-class.md) object,
and a two-panel plot is created with
[`plot,ladp-method()`](https://dankelley.github.io/oce/reference/plot-ladp-method.md)
to show velocity variation with pressure.

## Usage

``` r
# S4 method for class 'bremen'
plot(x, type, ...)
```

## Arguments

- x:

  a [bremen](https://dankelley.github.io/oce/reference/bremen-class.md)
  object.

- type:

  Optional string indicating the type to which `x` should be coerced
  before plotting. The choices are `ctd` and `ladp`.

- ...:

  Other arguments, passed to plotting functions.

## See also

Other functions that plot oce data:
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
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

Other things related to bremen data:
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
`[[<-,bremen-method`,
[`bremen-class`](https://dankelley.github.io/oce/reference/bremen-class.md),
[`read.bremen()`](https://dankelley.github.io/oce/reference/read.bremen.md),
[`summary,bremen-method`](https://dankelley.github.io/oce/reference/summary-bremen-method.md)

## Author

Dan Kelley
