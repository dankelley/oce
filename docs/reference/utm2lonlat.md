# Convert UTM to Longitude and Latitude

Convert UTM to Longitude and Latitude

## Usage

``` r
utm2lonlat(easting, northing, zone = 1, hemisphere = "N", km = FALSE)
```

## Arguments

- easting:

  easting coordinate (in km or m, depending on value of `km`).
  Alternatively, a list containing items named `easting`, `northing`,
  and `zone`, in which case these are taken from the list and the
  arguments named `northing`, `zone` and are ignored.

- northing:

  northing coordinate (in km or m, depending on value of `km`).

- zone:

  UTM zone

- hemisphere:

  indication of hemisphere; `"N"` for North, anything else for South.

- km:

  logical value indicating whether `easting` and `northing` are in
  kilometers or meters.

## Value

`utm2lonlat` returns a list containing `longitude` and `latitude`.

## References

`https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system`,
downloaded May 31, 2014.

## See also

[`lonlat2utm()`](https://dankelley.github.io/oce/reference/lonlat2utm.md)
does the inverse operation. For general projections and their inverses,
use
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md)
and
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
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
[`usrLonLat()`](https://dankelley.github.io/oce/reference/usrLonLat.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Cape Split, in the Minas Basin of the Bay of Fundy
utm2lonlat(852863, 5029997, 19)
#> $longitude
#> [1] -64.49657
#> 
#> $latitude
#> [1] 45.33462
#> 
```
