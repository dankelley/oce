# Add a ctd Profile to a section Object

Add a CTD profile to an existing section.

## Usage

``` r
sectionAddStation(section, station)
```

## Arguments

- section:

  A section to which a station is to be added.

- station:

  A ctd object holding data for the station to be added.

## Value

A [section](https://dankelley.github.io/oce/reference/section-class.md)
object.

## Historical note

Until March 2015, this operation was carried out with the `+` operator,
but at that time, the syntax was flagged by the development version of
R, so it was changed to the present form.

## See also

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
ctd2 <- ctd
ctd2[["temperature"]] <- ctd2[["temperature"]] + 0.5
ctd2[["latitude"]] <- ctd2[["latitude"]] + 0.1
section <- as.section(c("ctd", "ctd2"))
ctd3 <- ctd
ctd3[["temperature"]] <- ctd[["temperature"]] + 1
ctd3[["latitude"]] <- ctd[["latitude"]] + 0.1
ctd3[["station"]] <- "Stn 3"
sectionAddStation(section, ctd3)
#> Unnamed section has 3 stations:
#> Index    ID      Lon      Lat  Levels Depth
#>     1 Stn 2  -63.644   44.684     181    NA
#>     2 Stn 2  -63.644   44.784     181    NA
#>     3 Stn 3  -63.644   44.784     181    NA
```
