# Create a Section

Create a section based on columnar data, or a set of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects
that can be coerced to a section. There are three cases.

## Usage

``` r
as.section(
  salinity,
  temperature,
  pressure,
  longitude,
  latitude,
  station,
  sectionId = "",
  debug = getOption("oceDebug")
)
```

## Arguments

- salinity:

  This may be a numerical vector, in which case it is interpreted as the
  salinity, and the other arguments are used for the other components of
  [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects.
  Alternatively, it may be one of a variety of other objects from which
  the CTD objects can be inferred, in which case the other arguments are
  ignored; see “Details”.

- temperature:

  Temperature, in a vector holding values for all stations.

- pressure:

  Pressure, in a vector holding values for all stations.

- longitude:

  Longitude, in a vector holding values for all stations.

- latitude:

  Latitude, in a vector holding values for all stations.

- station:

  Station identifiers, in a vector holding values for all stations.

- sectionId:

  Section identifier.

- debug:

  an integer value that controls whether `as.section()` prints
  information during its work. The function works quietly if this is 0
  and prints out some information if it is positive.

## Value

An object of
[section](https://dankelley.github.io/oce/reference/section-class.md).

## Details

Case 1. If the first argument is a numerical vector, then it is taken to
be the salinity, and [`factor()`](https://rdrr.io/r/base/factor.html) is
applied to `station` to break the data up into chunks that are assembled
into [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
objects with
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) and
combined to make a
[section](https://dankelley.github.io/oce/reference/section-class.md)
object to be returned. This mode of operation is provided as a
convenience for datasets that are already partly processed; if original
CTD data are available, the next mode is preferred, because it permits
the storage of much more data and metadata in the CTD object.

Case 2. If the first argument is a list containing oce objects, then
those objects are taken as profiles of something. A requirement for this
to work is that every element of the list contains both `longitude` and
`latitude` in either the `metadata` or `data` slot (in the latter case,
the mean value is recorded in the section object) and that every element
also contains `pressure` in its `data` slot.

Case 3. If the first argument is a
[argo](https://dankelley.github.io/oce/reference/argo-class.md) object,
then the profiles it contains are turned into
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) objects,
and these are assembled into a section to be returned.

## See also

Other things related to section data:
[`[[,section-method`](https://dankelley.github.io/oce/reference/sub-sub-section-method.md),
`[[<-,section-method`,
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
library(oce)
data(ctd)
# vector of names of CTD objects
fake <- ctd
fake[["temperature"]] <- ctd[["temperature"]] + 0.5
fake[["salinity"]] <- ctd[["salinity"]] + 0.1
fake[["longitude"]] <- ctd[["longitude"]] + 0.01
fake[["station"]] <- "fake"
sec1 <- as.section(c("ctd", "fake"))
summary(sec1)
#> Section Summary
#> ---------------
#> 
#> * Source: ""
#> * ID:     ""
#> * Overview of stations
#> Index    ID       Lon       Lat  Levels Depth             Time
#>     1 Stn 2  -63.6439   44.6843     181    NA 2003-10-15 15:38
#>     2  fake  -63.6339   44.6843     181    NA 2003-10-15 15:38
#> * Processing Log
#> 
#>     - 2026-03-16 12:25:42 UTC: `create 'section' object`
#>     - 2026-03-16 12:25:42 UTC: `as.section(salinity = c("ctd", "fake"))`
# vector of CTD objects
ctds <- vector("list", 2)
ctds[[1]] <- ctd
ctds[[2]] <- fake
sec2 <- as.section(ctds)
#> Warning: estimated waterDepth as max(pressure) for CTDs numbered 1:2
summary(sec2)
#> Section Summary
#> ---------------
#> 
#> * Source: ""
#> * ID:     ""
#> * Overview of stations
#> Index    ID       Lon       Lat  Levels Depth             Time
#>     1 Stn 2  -63.6439   44.6843     181    44 2003-10-15 15:38
#>     2  fake  -63.6339   44.6843     181    44 2003-10-15 15:38
#> * Processing Log
#> 
#>     - 2026-03-16 12:25:42 UTC: `create 'section' object`
#>     - 2026-03-16 12:25:42 UTC: `as.section(salinity = ctds)`
# argo data (a subset)
data(argo)
sec3 <- as.section(subset(argo, profile < 5))
summary(sec3)
#> Section Summary
#> ---------------
#> 
#> * Source: ""
#> * ID:     ""
#> * Overview of stations
#> Index    ID       Lon       Lat  Levels Depth             Time
#>     1     1  -21.3850   60.9640      56    NA 2005-10-29 13:57
#>     2     2  -21.8880   60.8480      56    NA 2005-11-08 13:53
#>     3     3  -22.2570   61.1060      56    NA 2005-11-18 13:49
#>     4     4  -22.4160   61.2520      56    NA 2005-11-28 14:12
#> * Processing Log
#> 
#>     - 2026-03-16 12:25:42 UTC: `create 'section' object`
#>     - 2026-03-16 12:25:42 UTC: `as.section(salinity = subset(argo, profile < 5))`
```
