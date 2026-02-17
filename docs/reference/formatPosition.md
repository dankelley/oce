# Format Geographical Position in Degrees and Minutes

Format geographical positions to degrees, minutes, and hemispheres

## Usage

``` r
formatPosition(
  latlon,
  isLat = TRUE,
  type = c("list", "string", "expression"),
  showHemi = TRUE
)
```

## Arguments

- latlon:

  a vector of latitudes or longitudes

- isLat:

  a boolean that indicates whether the quantity is latitude or longitude

- type:

  a string indicating the type of return value (see below)

- showHemi:

  a boolean that indicates whether to indicate the hemisphere

## Value

A list containing `degrees`, `minutes`, `seconds`, and `hemispheres`, or
a vector of strings or (broken) a vector of expressions.

## See also

Other functions related to maps:
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md),
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md),
[`mapArrows()`](https://dankelley.github.io/oce/reference/mapArrows.md),
[`mapAxis()`](https://dankelley.github.io/oce/reference/mapAxis.md),
[`mapContour()`](https://dankelley.github.io/oce/reference/mapContour.md),
[`mapCoordinateSystem()`](https://dankelley.github.io/oce/reference/mapCoordinateSystem.md),
[`mapDirectionField()`](https://dankelley.github.io/oce/reference/mapDirectionField.md),
[`mapGrid()`](https://dankelley.github.io/oce/reference/mapGrid.md),
[`mapImage()`](https://dankelley.github.io/oce/reference/mapImage.md),
[`mapLines()`](https://dankelley.github.io/oce/reference/mapLines.md),
[`mapLocator()`](https://dankelley.github.io/oce/reference/mapLocator.md),
[`mapLongitudeLatitudeXY()`](https://dankelley.github.io/oce/reference/mapLongitudeLatitudeXY.md),
[`mapPlot()`](https://dankelley.github.io/oce/reference/mapPlot.md),
[`mapPoints()`](https://dankelley.github.io/oce/reference/mapPoints.md),
[`mapPolygon()`](https://dankelley.github.io/oce/reference/mapPolygon.md),
[`mapScalebar()`](https://dankelley.github.io/oce/reference/mapScalebar.md),
[`mapText()`](https://dankelley.github.io/oce/reference/mapText.md),
[`mapTissot()`](https://dankelley.github.io/oce/reference/mapTissot.md),
[`oceCRS()`](https://dankelley.github.io/oce/reference/oceCRS.md),
[`oceProject()`](https://dankelley.github.io/oce/reference/oceProject.md),
[`shiftLongitude()`](https://dankelley.github.io/oce/reference/shiftLongitude.md),
[`usrLonLat()`](https://dankelley.github.io/oce/reference/usrLonLat.md),
[`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
formatPosition(10 + 1:10 / 60 + 2.8 / 3600)
#> expression(c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10), c(1, 2, 
#> 3, 4, 5, 6, 7, 8, 9, 10), c(2.8, 2.8, 2.8, 2.8, 2.8, 2.8, 2.8, 
#> 2.8, 2.8, 2.8), c("N", NA, NA, NA, NA, NA, NA, NA, NA, NA), NULL, 
#>     NULL, NULL, NULL, NULL, NULL)
formatPosition(10 + 1:10 / 60 + 2.8 / 3600, type = "string")
#> expression("10 01' 2.80\" N", "10 02' 2.80\" NA", "10 03' 2.80\" NA", 
#>     "10 04' 2.80\" NA", "10 05' 2.80\" NA", "10 06' 2.80\" NA", 
#>     "10 07' 2.80\" NA", "10 08' 2.80\" NA", "10 09' 2.80\" NA", 
#>     "10 10' 2.80\" NA")
```
