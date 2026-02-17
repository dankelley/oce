# Find the Name of the Best Coastline Object

Find the name of the most appropriate coastline for a given locale
Checks `coastlineWorld`, `coastlineWorldFine` and
`coastlineWorldCoarse`, in that order, to find the one most appropriate
for the locale.

## Usage

``` r
coastlineBest(lonRange, latRange, span, debug = getOption("oceDebug"))
```

## Arguments

- lonRange:

  range of longitude for locale

- latRange:

  range of latitude for locale

- span:

  span of domain in km (if provided, previous two arguments are
  ignored).

- debug:

  set to a positive value to get debugging information during
  processing.

## Value

The name of a coastline that can be loaded with
[`data()`](https://rdrr.io/r/utils/data.html).

## See also

Other things related to coastline data:
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
`[[<-,coastline-method`,
[`as.coastline()`](https://dankelley.github.io/oce/reference/as.coastline.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`coastlineCut()`](https://dankelley.github.io/oce/reference/coastlineCut.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`plot,coastline-method`](https://dankelley.github.io/oce/reference/plot-coastline-method.md),
[`read.coastline.openstreetmap()`](https://dankelley.github.io/oce/reference/read.coastline.openstreetmap.md),
[`read.coastline.shapefile()`](https://dankelley.github.io/oce/reference/read.coastline.shapefile.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`summary,coastline-method`](https://dankelley.github.io/oce/reference/summary-coastline-method.md)

## Author

Dan Kelley
