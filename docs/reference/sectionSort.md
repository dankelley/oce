# Sort a Section

Sections created with
[`as.section()`](https://dankelley.github.io/oce/reference/as.section.md)
have "stations" that are in the order of the CTD objects (or filenames
for such objects) provided. Sometimes, this is not the desired order,
e.g. if file names discovered with
[`dir()`](https://rdrr.io/r/base/list.files.html) are in an order that
makes no sense. (For example, a practioner might name stations `"stn1"`,
`"stn2"`, etc., not realizing that this will yield an unhelpful
ordering, by file name, if there are more than 9 stations.) The purpose
of `sectionSort` is to permit reordering the constituent stations in
sensible ways.

## Usage

``` r
sectionSort(section, by, decreasing = FALSE)
```

## Arguments

- section:

  A `section` object containing the section whose stations are to be
  sorted.

- by:

  An optional string indicating how to reorder. If not provided,
  `"stationID"` will be assumed. Other choices are `"distance"`, for
  distance from the first station, `"longitude"`, for longitude,
  `"latitude"` for latitude, and `"time"`, for time.

- decreasing:

  A logical value indicating whether to sort in decreasing order. The
  default is FALSE. (Thanks to Martin Renner for adding this parameter.)

## Value

object A
[section](https://dankelley.github.io/oce/reference/section-class.md)
object that has been smoothed, so its data fields will
station-to-station variation than is the case for the input section,
`x`.

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
[`sectionAddStation()`](https://dankelley.github.io/oce/reference/sectionAddStation.md),
[`sectionGrid()`](https://dankelley.github.io/oce/reference/sectionGrid.md),
[`sectionSmooth()`](https://dankelley.github.io/oce/reference/sectionSmooth.md),
[`subset,section-method`](https://dankelley.github.io/oce/reference/subset-section-method.md),
[`summary,section-method`](https://dankelley.github.io/oce/reference/summary-section-method.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(section)
sectionByLongitude <- sectionSort(section, by = "longitude")
head(section)
#> Section 'a03' has 6 stations:
#> Index    ID      Lon      Lat  Levels Depth
#>     1     3   -8.526   36.876       5   202
#>     2     4   -8.620   36.719      16   742
#>     3     6   -8.738   36.538      19  1567
#>     4     7   -8.791   36.425      24  2349
#>     5     8   -8.901   36.250      23  2629
#>     6     9   -9.271   36.235      24  3842
head(sectionByLongitude)
#> Section 'a03' has 6 stations:
#> Index    ID      Lon      Lat  Levels Depth
#>     1   133  -73.673   38.237       8   154
#>     2   132  -73.605   38.205      13   843
#>     3   131  -73.491   38.158      16  1378
#>     4   130  -73.319   38.086      20  2024
#>     5   129  -73.156   38.004      22  2363
#>     6   128  -72.976   37.925      24  2574
```
