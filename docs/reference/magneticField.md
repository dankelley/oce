# Earth Magnetic Declination, Inclination, and Intensity

Implements the 12th, 13th and 14th generations of the International
Geomagnetic Reference Field (IGRF), based on a reworked version of a
Fortran code downloaded from a NOAA website (see “References”).

## Usage

``` r
magneticField(longitude, latitude, time, version = 14)
```

## Arguments

- longitude:

  longitude in degrees east (negative for degrees west), as a number, a
  vector, or a matrix.

- latitude:

  latitude in degrees north, as a number, vector, or matrix. The shape
  (length or dimensions) must conform to the dimensions of `longitude`.

- time:

  The time at which the field is desired. This may be a single value or
  a vector or matrix that is structured to match `longitude` and
  `latitude`. The value may a decimal year, a POSIXt time, or a Date
  time.

- version:

  an integer that must be 12, 13 or 14, to specify the version number of
  the formulae. Note that 13 became the default on 2020 March 3 and that
  14 became the default on 2025 January 7.

## Value

A list containing `declination`, `inclination`, and `intensity`.

## Details

The Fortran code (subroutines `igrf12syn`, `igrf13syn` and `igrf14syn`)
seem to have been written by Susan Macmillan of the British Geological
Survey. Comments in the source code `igrf14syn` (the current default
used here) indicate that its coefficients were agreed to in November
2024 by the IAGA Working Group V-MOD. Other comments in that code
suggest that the proposed application time interval is from years 1900
to 2030, inclusive, but that only dates from 1945 to 2020 are to be
considered definitive.

## References

1.  The underlying Fortran code for version 12 is from `igrf12.f`,
    downloaded the NOAA website
    `https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html` on June 7, 2015. For
    version 13, `igrf13.f` was downloaded from the NOAA website
    `https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html` on March 3, 2020.
    For version 14, `igrf14.f` was downloaded from the NOAA website
    `https://www.ngdc.noaa.gov/IAGA/vmod/igrf14.f` on January 7, 2025.

2.  Witze, Alexandra. “Earth's Magnetic Field Is Acting up and
    Geologists Don't Know Why.” Nature 565 (January 9, 2019): 143.
    [doi:10.1038/d41586-019-00007-1](https://doi.org/10.1038/d41586-019-00007-1)

3.  Alken, P., E. Thébault, C. D. Beggan, H. Amit, J. Aubert, J.
    Baerenzung, T. N. Bondar, et al. "International Geomagnetic
    Reference Field: The Thirteenth Generation." Earth, Planets and
    Space 73, no. 1 (December 2021): 49.
    [doi:10.1186/s40623-020-01288-x](https://doi.org/10.1186/s40623-020-01288-x)
    .

4.  Chulliat, Arnaud, Manoj nair, Li-Yin Young, et al. The US/UK World
    Magnetic Model for 2025-2030. NOAA National Centers for
    Environmental Information, 2025.
    [doi:10.25923/PRBC-S316](https://doi.org/10.25923/PRBC-S316) .

## See also

Other things related to magnetism:
[`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md),
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`applyMagneticDeclination,oce-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-oce-method.md)

## Author

Dan Kelley wrote the R code and a fortran wrapper to the `igrf1X.f`
subroutine (where X can be 12, 13 or 14), which was written by Susan
Macmillan of the British Geological Survey and distributed “without
limitation” (email from SM to DK dated June 5, 2015). This version was
updated subsequent to that date; see “Historical Notes”.

## Examples

``` r
library(oce)
# 1. Today's value at Halifax NS
magneticField(-(63 + 36 / 60), 44 + 39 / 60, Sys.Date())
#> $declination
#> [1] -16.1715
#> 
#> $inclination
#> [1] 66.29049
#> 
#> $intensity
#> [1] 51040.47
#> 

# 2. World map of declination in year 2025.
# \donttest{
data(coastlineWorld)
par(mar = rep(0.5, 4)) # no axes on whole-world projection
mapPlot(coastlineWorld, projection = "+proj=robin", col = "lightgray")
# Construct matrix holding declination
lon <- seq(-180, 180)
lat <- seq(-90, 90)
dec2025 <- function(lon, lat) {
    magneticField(lon, lat, 2025)$declination
}
dec <- outer(lon, lat, dec2025) # hint: outer() is very handy!
# Contour, unlabelled for small increments, labeled for
# larger increments.
mapContour(lon, lat, dec,
    col = "blue", levels = seq(-180, -5, 5),
    lty = 3, drawlabels = FALSE
)
mapContour(lon, lat, dec, col = "blue", levels = seq(-180, -20, 20))
mapContour(lon, lat, dec,
    col = "red", levels = seq(5, 180, 5),
    lty = 3, drawlabels = FALSE
)
mapContour(lon, lat, dec, col = "red", levels = seq(20, 180, 20))
mapContour(lon, lat, dec, levels = 180, col = "black", lwd = 2, drawlabels = FALSE)
mapContour(lon, lat, dec, levels = 0, col = "black", lwd = 2)

# }

# 3. Declination differences between versions 13 and 14
lon <- seq(-180, 180)
lat <- seq(-90, 90)
decDiff <- function(lon, lat) {
    old <- magneticField(lon, lat, 2025, version = 13)$declination
    new <- magneticField(lon, lat, 2025, version = 14)$declination
    new - old
}
decDiff <- outer(lon, lat, decDiff)
t.test(decDiff)
#> 
#>  One Sample t-test
#> 
#> data:  decDiff
#> t = -3.0533, df = 65340, p-value = 0.002264
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  -0.038733905 -0.008446985
#> sample estimates:
#>   mean of x 
#> -0.02359044 
#> 
```
