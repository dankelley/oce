# Extract Something From a section Object

Generally, the `[[` method lets users extract information from `oce`
objects, without having to know the details of the internal storage. For
many `oce` sub-classes, `[[` can also return quantities that are
computed from the object's contents.

## Usage

``` r
# S4 method for class 'section'
x[[i, j, ...]]
```

## Arguments

- x:

  a
  [section](https://dankelley.github.io/oce/reference/section-class.md)
  object.

- i:

  character value indicating the name of an item to extract.

- j:

  optional additional information on the `i` item.

- ...:

  ignored.

## Details

A two-step process is used to try to find the requested information.
First, a class-specific function is used (see “Details of the
Specialized Method”). If this yields nothing, then a general method is
used (see “Details of the General Method”). If both methods fail, then
`[[` returns NULL.

Some understanding of the subclass is required to know what can be
retrieved with `[[`. When dealing with an unfamiliar subclass, it can be
useful to first use `x[["?"]]` to get a listing of the retrievable
items. See “Details of the Specialized Method” for more information.

## Details of the Specialized Method

There are several possibilities, depending on the nature of `i`.

- If `i` is `"?"`, then the return value is a list containing four
  items, each of which is a character vector holding the names of things
  that can be accessed with `[[`. This list is compiled by examining all
  the stations in the object, and reporting an entry if it is found in
  any one of them. The `data` and `metadata` items hold the names of
  entries in the object's data and metadata slots, respectively. The
  `dataDerived` and `metadataDerived` items hold data-like and
  metadata-like things that can be derived from these.

- If `i` is `"station"`, then `[[` will return a
  [`list()`](https://rdrr.io/r/base/list.html) of
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects
  holding the station data. If `j` is also given, it specifies a station
  (or set of stations) to be returned. if `j` contains just a single
  value, then that station is returned, but otherwise a list is
  returned. If `j` is an integer, then the stations are specified by
  index, but if it is character, then stations are specified by the
  names stored within their metadata. (Missing stations yield `NULL` in
  the return value.)

- If `i` is `"station ID"`, then the IDs of the stations in the section
  are returned.

- If `i` is `"dynamic height"`, then an estimate of dynamic height is
  returned, as calculated with
  [`swDynamicHeight`](https://dankelley.github.io/oce/reference/swDynamicHeight.md)`(x)`.

- If `i` is `"distance"`, then the distance along the section is
  returned, using
  [`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md).

- If `i` is `"depth"`, then a vector containing the depths of the
  stations is returned.

- If `i` is `"z"`, then a vector containing the z coordinates is
  returned.

- If `i` is `"theta"` or `"potential temperature"`, then the potential
  temperatures of all the stations are returned in one vector.
  Similarly, `"spice"` returns the property known as spice, using
  [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md).

- If `i` is a string ending with `"Flag"`, then the characters prior to
  that ending are taken to be the name of a variable contained within
  the stations in the section. If this flag is available in the first
  station of the section, then the flag values are looked up for every
  station.

If `j` is `"byStation"`, then a list is returned, with one (unnamed)
item per station.

If `j` is `"grid:distance-pressure"` or `"grid:time-pressure"`, then a
gridded representation of `i` is returned, as a list with elements:
`distance` (in km) or `time` (in POSIXct); `pressure` (in dbar) and
`field` (in whatever unit is used for `i`). See the examples in the
documentation for
[`plot,section-method()`](https://dankelley.github.io/oce/reference/plot-section-method.md).

## Details of the General Method

Note: the text of this section is identical for all `oce` subclasses,
and so some of what you read here may not be relevant to the class being
described in this help page.

If the specialized method produces no matches, the following generalized
method is applied. As with the specialized method, the procedure hinges
first on the values of `i` and, optionally, `j`. The work proceeds in
steps, by testing a sequence of possible conditions in sequence.

1.  A check is made as to whether `i` names one of the standard `oce`
    slots. If so, `[[` returns the slot contents of that slot. Thus,
    `x[["metadata"]]` will retrieve the `metadata` slot, while
    `x[["data"]]` and `x[["processingLog"]]` return those slots.

2.  If `i` is a string ending in the `"Unit"`, then the characters
    preceding that string are taken to be the name of an item in the
    data object, and a list containing the unit is returned (or `NULL`
    if there is no such unit). This list consists of an item named
    `unit`, which is an
    [`expression()`](https://rdrr.io/r/base/expression.html), and an
    item named `scale`, which is a string describing the measurement
    scale. If the string ends in `" unit"`, e.g.
    `x[["temperature unit"]]` (note the space), then just the expression
    is returned, and if it ends in `" scale"`, then just the scale is
    returned.

3.  If `i` is a string ending in `"Flag"`, then the corresponding
    data-quality flag is returned (or `NULL` if there is no such flag).

4.  If the object holds hydrographic information (pressure, salinity,
    temperature, longitude and latitude) then another set of
    possibilities arises. If `i` is `"sigmaTheta"`, then the value of
    [`swSigmaTheta()`](https://dankelley.github.io/oce/reference/swSigmaTheta.md)
    is called with `x` as the sole argument, and the results are
    returned. Similarly,
    [`swSigma0()`](https://dankelley.github.io/oce/reference/swSigma0.md)
    is used if `i="sigma0"`, and
    [`swSpice()`](https://dankelley.github.io/oce/reference/swSpice.md)
    is used if `i="spice"`. Of course, these actions only make sense for
    objects that contain the relevant items within their `data` slot.

5.  After these possibilities are eliminated, the action depends on
    whether `j` has been provided. If `j` is not provided, or is the
    string `""`, then `i` is sought in the `metadata` slot, and then in
    the `data` slot, returning whichever is found first. In other words,
    if `j` is not provided, the `metadata` slot takes preference over
    the `data` slot. However, if `j` is provided, then it must be either
    the string `"metadata"` or `"data"`, and it directs where to look.

6.  If none of the above-listed conditions holds, then `NULL` is
    returned.

## See also

Other functions that extract parts of oce objects:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
[`[[,coastline-method`](https://dankelley.github.io/oce/reference/sub-sub-coastline-method.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
[`[[,g1sst-method`](https://dankelley.github.io/oce/reference/sub-sub-g1sst-method.md),
[`[[,gps-method`](https://dankelley.github.io/oce/reference/sub-sub-gps-method.md),
[`[[,ladp-method`](https://dankelley.github.io/oce/reference/sub-sub-ladp-method.md),
[`[[,landsat-method`](https://dankelley.github.io/oce/reference/sub-sub-landsat-method.md),
[`[[,lisst-method`](https://dankelley.github.io/oce/reference/sub-sub-lisst-method.md),
[`[[,lobo-method`](https://dankelley.github.io/oce/reference/sub-sub-lobo-method.md),
[`[[,met-method`](https://dankelley.github.io/oce/reference/sub-sub-met-method.md),
[`[[,oce-method`](https://dankelley.github.io/oce/reference/sub-sub-oce-method.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
[`[[,topo-method`](https://dankelley.github.io/oce/reference/sub-sub-topo-method.md),
[`[[,windrose-method`](https://dankelley.github.io/oce/reference/sub-sub-windrose-method.md),
[`[[,xbt-method`](https://dankelley.github.io/oce/reference/sub-sub-xbt-method.md),
`[[<-,adv-method`

Other things related to section data: `[[<-,section-method`,
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
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
data(section)
length(section[["latitude"]])
#> [1] 2841
length(section[["latitude", "byStation"]])
#> [1] 124
# Vector of all salinities, for all stations
Sv <- section[["salinity"]]
# List of salinities, grouped by station
Sl <- section[["salinity", "byStation"]]
# First station salinities
Sl[[1]]
#> [1] 36.1384 36.1103 35.9953 35.9372 35.8191
```
