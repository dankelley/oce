# Convert Longitude and Latitude to UTM

Convert Longitude and Latitude to UTM

## Usage

``` r
lonlat2utm(longitude, latitude, zone, km = FALSE)
```

## Arguments

- longitude:

  numeric vector of decimal longitude. May also be a list containing
  items named `longitude` and `latitude`, in which case the indicated
  values are used, and next argument is ignored.

- latitude:

  numeric vector of decimal latitude (ignored if `longitude` is a list
  containing both coordinates)

- zone:

  optional indication of UTM zone. Normally this is inferred from the
  longitude, but specifying it can be helpful in dealing with Landsat
  images, which may cross zones and which therefore are described by a
  single zone.

- km:

  logical value indicating whether `easting` and `northing` are in
  kilometers or meters.

## Value

`lonlat2utm` returns a list containing `easting`, `northing`, `zone` and
`hemisphere`.

## References

`https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system`,
downloaded May 31, 2014.

## See also

[`utm2lonlat()`](https://dankelley.github.io/oce/reference/utm2lonlat.md)
does the inverse operation. For general projections and their inverses,
use
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md)
and
[`map2lonlat()`](https://dankelley.github.io/oce/reference/map2lonlat.md).

Other functions related to maps:
[`formatPosition()`](https://dankelley.github.io/oce/reference/formatPosition.md),
[`lonlat2map()`](https://dankelley.github.io/oce/reference/lonlat2map.md),
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
# Cape Split, in the Minas Basin of the Bay of Fundy
lonlat2utm(-64.496567, 45.334626)
#> $easting
#> [1] 382736.4
#> 
#> $northing
#> [1] 5021214
#> 
#> $zone
#> [1] 20
#> 
#> $hemisphere
#> [1] "N"
#> 
```
