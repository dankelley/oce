# Subset a section Object

Return a subset of a section object.

## Usage

``` r
# S4 method for class 'section'
subset(x, subset, ...)
```

## Arguments

- x:

  a
  [section](https://dankelley.github.io/oce/reference/section-class.md)
  object.

- subset:

  an optional indication of either the stations to be kept, or the data
  to be kept within the stations. See “Details”.

- ...:

  optional arguments, of which only the first is examined. The
  possibilities for this argument are `indices`, which must be a vector
  of station indices (see Example 6), or `within`, which must be a list
  or data frame, containing items named either `x` and `y` or
  `longitude` and `latitude` (see Example 7). If `within` is given, then
  `subset` is ignored.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object.

## Details

This function is used to subset data within the stations of a section,
or to choose a subset of the stations themselves. The first case is
handled with the `subset` argument, while the second is handled if `...`
contains a vector named `indices`. Either `subset` or `indices` must be
provided, but not both.

**In the "subset" method**, `subset` indicates either stations to be
kept, or data to be kept within the stations.

The first step in processing is to check for the presence of certain key
words in the `subset` expression. If `distance` is present, then
stations are selected according to a condition on the distance (in km)
from the first station to the given station (Example 1). If either
`longitude` or `latitude` is given, then stations are selected according
to the stated condition (Example 2). If `stationId` is present, then
selection is in terms of the station ID (not the sequence number) is
used (Example 3). In all of these cases, stations are either selected in
their entirety or dropped in their entirety.

If none of these keywords is present, then the `subset` expression is
evaluated in the context of the `data` slot of each of the CTD stations
stored within `x`. (Note that this slot does not normally contain any of
the keywords that are listed in the previous paragraph; it does, then
odd results may occur.) Each station is examined in turn, with `subset`
being evaluated individually in each. The evaluation produces a logical
vector. If that vector has length 1 (Examples 4 and 5) then the station
is retained or discarded, accordingly. If the vector is longer, then the
logical vector is used as a sieve to subsample that individual CTD
profile.

**In the "indices" method**, stations are selected using `indices`,
which may be a vector of integers that indicate sequence number, or a
logical vector, again indicating which stations to keep.

## Sample of Usage

    # Example 7. Subset by a polygon determined with locator()
    par(mfrow=c(2, 1))
    plot(section, which="map")
    bdy <- locator(4) # choose a polygon near N. America
    GS <- subset(section, within=bdy)
    plot(GS, which="map")

## See also

Other functions that subset oce objects:
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`subset,coastline-method`](https://dankelley.github.io/oce/reference/subset-coastline-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`subset,lobo-method`](https://dankelley.github.io/oce/reference/subset-lobo-method.md),
[`subset,met-method`](https://dankelley.github.io/oce/reference/subset-met-method.md),
[`subset,oce-method`](https://dankelley.github.io/oce/reference/subset-oce-method.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`subset,topo-method`](https://dankelley.github.io/oce/reference/subset-topo-method.md),
[`subset,xbt-method`](https://dankelley.github.io/oce/reference/subset-xbt-method.md)

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
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`sectionSort()`](https://dankelley.github.io/oce/reference/sectionSort.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(section)

# Example 1. Stations within 500 km of the first station
starting <- subset(section, distance < 500)

# Example 2. Stations east of 50W
east <- subset(section, longitude > (-50))

# Example 3. Gulf Stream
GS <- subset(section, 113 <= stationId & stationId <= 129)

# Example 4. Only stations with more than 5 pressure levels
long <- subset(section, length(pressure) > 5)

# Example 5. Only stations that have some data in top 50 dbar
surfacing <- subset(section, min(pressure) < 50)

# Example 6. Similar to #4, but done in more detailed way
long <- subset(section,
    indices = unlist(lapply(
        section[["station"]],
        function(s) 5 < length(s[["pressure"]])
    ))
)
```
