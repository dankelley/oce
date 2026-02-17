# Base Class for oce Objects

This is mainly used within oce to create sub-classes, although users may
wish to make direct calls to `new("oce")` for their own purposes.

## Slots

- `metadata`:

  A list containing information about the data. The contents vary across
  sub-classes, e.g. an
  [adp](https://dankelley.github.io/oce/reference/adp-class.md) object
  has information about beam patterns, which obviously would not make
  sense for a
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object
  In addition, all classes have items named `units` and `flags`, used to
  store information on the units of the data, and the data quality.

- `data`:

  A list containing the data.

- `processingLog`:

  A list containing time-stamped processing steps, typically stored in
  the object by oce functions.

## See also

Other classes provided by oce:
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`bremen-class`](https://dankelley.github.io/oce/reference/bremen-class.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

## Examples

``` r
str(new("oce"))
#> Formal class 'oce' [package "oce"] with 3 slots
#>   ..@ metadata     :List of 2
#>   .. ..$ units: list()
#>   .. ..$ flags: list()
#>   ..@ data         : list()
#>   ..@ processingLog:List of 2
#>   .. ..$ time : POSIXct[1:1], format: "2026-02-17 20:57:22"
#>   .. ..$ value: chr "Create oce object"
```
