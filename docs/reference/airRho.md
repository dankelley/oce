# Air Density

Compute \\\rho\\, the *in-situ* density of dry air.

## Usage

``` r
airRho(temperature, pressure, humidity)
```

## Arguments

- temperature:

  *in-situ* temperature, in \\^\circ\\C.

- pressure:

  numeric value for pressure in Pa (*not* the kPa used in public weather
  forecasts).

- humidity:

  ignored at present

## Value

*In-situ* dry-air density, in kg/m\\^3\\.

## Details

This will eventually be a proper equation of state, but for now it just
uses a dry-air formula posted on wikipedia (i.e. not trustworthy).

## References

1.  `https://en.wikipedia.org/wiki/Density_of_air`

2.  National Oceanographic and Atmospheric Agency, 1976. U.S. Standard
    Atmosphere, 1976. NOAA-S/T 76-1562. (A PDF of this document may be
    available at
    `http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf`
    or
    `http://www.dtic.mil/cgi-bin/GetTRDoc?Location=U2&doc=GetTRDoc.pdf&AD=ADA035728`
    although neither link has proven to be reliable.)

## Author

Dan Kelley

## Examples

``` r
degC <- seq(0, 30, length.out = 100)
p <- seq(98, 102, length.out = 100) * 1e3
contour(x = degC, y = p, z = outer(degC, p, airRho), labcex = 1)
```
