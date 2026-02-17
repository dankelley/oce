# Summarize an adv Object

Summarize data in an `adv` object.

## Usage

``` r
# S4 method for class 'adv'
summary(object, ...)
```

## Arguments

- object:

  an object of class `"adv"`, usually, a result of a call to
  [`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md).

- ...:

  further arguments passed to or from other methods.

## See also

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md),
[`applyMagneticDeclination,adv-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adv-method.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdv()`](https://dankelley.github.io/oce/reference/enuToOtherAdv.md),
[`plot,adv-method`](https://dankelley.github.io/oce/reference/plot-adv-method.md),
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
[`read.adv.nortek()`](https://dankelley.github.io/oce/reference/read.adv.nortek.md),
[`read.adv.sontek.adr()`](https://dankelley.github.io/oce/reference/read.adv.sontek.adr.md),
[`read.adv.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adv.sontek.serial.md),
[`read.adv.sontek.text()`](https://dankelley.github.io/oce/reference/read.adv.sontek.text.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`subset,adv-method`](https://dankelley.github.io/oce/reference/subset-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(adv)
summary(adv)
#> ADV Summary
#> -----------
#> 
#> * Instrument:    vector, serial number ``(serial number redacted)``
#> * Filename:      `(file name redacted)`
#> * Location:      47.87943 N ,  -69.72533 E 
#> * Time: 2008-07-01 00:00:00 to 2008-07-01 00:00:59 (480 samples, mean increment 0.1250001 s)
#> * Data Overview
#> 
#>                          Min.       Mean       Max.       Dim.    NAs
#>     v [m/s]              -0.080871  0.00069514 0.057789   "480x3" 0  
#>     a                    NA         NA         NA         "480x3" 0  
#>     q                    NA         NA         NA         "480x3" 0  
#>     time                 1214870400 1214870430 1214870460 480     0  
#>     pressure [dbar]      16.85      16.866     16.879     480     0  
#>     timeBurst            NA         NA         NA         480     480
#>     recordsBurst         NA         NA         NA         480     480
#>     voltageSlow          9.7        9.71       9.8        60      0  
#>     timeSlow             1214870401 1214870430 1214870460 60      0  
#>     headingSlow [°]      -23.39     -23.39     -23.39     60      0  
#>     pitchSlow [°]        0.4        0.5        0.6        60      0  
#>     rollSlow [°]         -6.2       -6.145     -6.1       60      0  
#>     temperatureSlow [°C] 6.47       6.4997     6.51       60      0  
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
```
