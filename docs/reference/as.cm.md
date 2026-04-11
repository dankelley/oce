# Coerce Data Into a cm Object

Coerce Data Into a cm Object

## Usage

``` r
as.cm(
  time,
  u = NULL,
  v = NULL,
  pressure = NULL,
  conductivity = NULL,
  temperature = NULL,
  salinity = NULL,
  longitude = NA,
  latitude = NA,
  filename = "",
  debug = getOption("oceDebug")
)
```

## Arguments

- time:

  A vector of times of observation, or an `oce` object from which time
  and two velocity components can be inferred, e.g. an
  [adv](https://dankelley.github.io/oce/reference/adv-class.md) object,
  or an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object that has only one distance bin. If `time` is an `oce` object,
  then all of the following arguments are ignored.

- u, v:

  optional numerical vectors containing the x and y components of
  velocity (m/s).

- pressure, conductivity, salinity, temperature:

  optional numerical vectors containing pressure (dbar), electrical
  conductivity, practical salinity, and in-situ temperature (degree C).

- longitude, latitude:

  optional position specified in degrees East and North.

- filename:

  optional source file name.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## See also

Other things related to cm data:
[`[[,cm-method`](https://dankelley.github.io/oce/reference/sub-sub-cm-method.md),
`[[<-,cm-method`,
[`applyMagneticDeclination,cm-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-cm-method.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`plot,cm-method`](https://dankelley.github.io/oce/reference/plot-cm-method.md),
[`read.cm()`](https://dankelley.github.io/oce/reference/read.cm.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,cm-method`](https://dankelley.github.io/oce/reference/subset-cm-method.md),
[`summary,cm-method`](https://dankelley.github.io/oce/reference/summary-cm-method.md)

## Examples

``` r
library(oce)
# Example 1: creation from scratch
t <- Sys.time() + 0:50
u <- sin(2 * pi * 0:50 / 5) + rnorm(51)
v <- cos(2 * pi * 0:50 / 5) + rnorm(51)
p <- 100 + rnorm(51)
summary(as.cm(t, u, v, p))
#> Cm summary
#> ----------
#> 
#> * North:         magnetic
#> * Time: 2026-04-11 08:44:56 to 2026-04-11 08:45:46 (51 samples, mean increment 1 s)
#> * Data Overview
#> 
#>                     Min.       Mean       Max.       Dim. NAs
#>     time            1775907897 1775907922 1775907947 51   0  
#>     u [m/s]         -3.5634    0.18818    2.6826     51   0  
#>     v [m/s]         -2.2084    0.033611   2.3168     51   0  
#>     pressure [dbar] 98.1       100.23     102.56     51   0  
#> 
#> * Processing Log
#> 
#>     - 2026-04-11 11:44:56 UTC: `create 'cm' object`
#>     - 2026-04-11 11:44:56 UTC: `as.cm(time = t, u = u, v = v, pressure = p)`

# Example 2: creation from an adv object
data(adv)
summary(as.cm(adv))
#> Cm summary
#> ----------
#> 
#> * File source:   "(file name redacted)"
#> * Serial Num.:   (serial number redacted)
#> * North:         magnetic
#> * Time: 2008-07-01 00:00:00 to 2008-07-01 00:00:59 (480 samples, mean increment 0.1250001 s)
#> * Data Overview
#> 
#>                     Min.       Mean       Max.       Dim. NAs
#>     time            1214870400 1214870430 1214870460 480  0  
#>     u [m/s]         -0.080871  -0.027275  0.012168   480  0  
#>     v [m/s]         -0.0090777 0.021877   0.057789   480  0  
#>     pressure [dbar] 16.85      16.866     16.879     480  0  
#> 
#> * Processing Log
#> 
#>     - 2015-12-23 17:53:39 UTC: `read.oce(file = "/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",     from = as.POSIXct("2008-06-25 00:00:00", tz = "UTC"), to = as.POSIXct("2008-07-06 00:00:00",         tz = "UTC"), latitude = 47.87943, longitude = -69.72533)`
#>     - 2015-12-23 17:53:54 UTC: `retime(x = m05VectorBeam, a = 0.58, b = 6.3892e-07, t0 = as.POSIXct("2008-07-01 00:00:00",     tz = "UTC"))`
#>     - 2015-12-23 17:53:55 UTC: `subset(x, subset=as.POSIXct("2008-06-25 13:00:00", tz = "UTC") <= time & time <=      as.POSIXct("2008-07-03 00:50:00", tz = "UTC"))`
#>     - 2015-12-23 17:53:55 UTC: `oceEdit(x = m05VectorBeam, item = "transformationMatrix", value = rbind(c(11033,     -5803, -5238), c(347, -9622, 9338), c(-1418, -1476, -1333))/4096,     reason = "Nortek email 2011-02-14", person = "DEK")`
#>     - 2015-12-23 17:53:55 UTC: `use aquadoppHR heading; despike own pitch and roll`
#>     - 2015-12-23 17:54:11 UTC: `beamToXyzAdv(x = x)`
#>     - 2015-12-23 17:54:34 UTC: `xyzToEnu(x, declination=-18.099, horizontalCase=TRUE, sensorOrientiation=upward, debug=0)`
#>     - 2026-04-11 11:44:56 UTC: `as.cm(time = adv)`
```
