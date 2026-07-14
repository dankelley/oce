# Convert Raw to Numeric Values in an adp Object

Convert variables in an
[adp](https://dankelley.github.io/oce/reference/adp-class.md) object
from raw to numeric format.

## Usage

``` r
adpConvertRawToNumeric(
  object = NULL,
  variables = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- object:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- variables:

  variables stored in an
  [adp](https://dankelley.github.io/oce/reference/adp-class.md) object
  that has the same dimensional as `v` and is stored in a raw format.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

`adpConvertRawToNumeric` returns an
[adp](https://dankelley.github.io/oce/reference/adp-class.md) object
whose specified variables have been converted from raw to numerical
format.

## See also

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpEnsembleAverage()`](https://dankelley.github.io/oce/reference/adpEnsembleAverage.md),
[`adpFlagPastBoundary()`](https://dankelley.github.io/oce/reference/adpFlagPastBoundary.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`adp_rdi.000`](https://dankelley.github.io/oce/reference/adp_rdi.000.md),
[`applyMagneticDeclination,adp-method`](https://dankelley.github.io/oce/reference/applyMagneticDeclination-adp-method.md),
[`as.adp()`](https://dankelley.github.io/oce/reference/as.adp.md),
[`beamName()`](https://dankelley.github.io/oce/reference/beamName.md),
[`beamToXyz()`](https://dankelley.github.io/oce/reference/beamToXyz.md),
[`beamToXyzAdp()`](https://dankelley.github.io/oce/reference/beamToXyzAdp.md),
[`beamToXyzAdpAD2CP()`](https://dankelley.github.io/oce/reference/beamToXyzAdpAD2CP.md),
[`beamToXyzAdv()`](https://dankelley.github.io/oce/reference/beamToXyzAdv.md),
[`beamUnspreadAdp()`](https://dankelley.github.io/oce/reference/beamUnspreadAdp.md),
[`binmapAdp()`](https://dankelley.github.io/oce/reference/binmapAdp.md),
[`enuToOther()`](https://dankelley.github.io/oce/reference/enuToOther.md),
[`enuToOtherAdp()`](https://dankelley.github.io/oce/reference/enuToOtherAdp.md),
[`handleFlags,adp-method`](https://dankelley.github.io/oce/reference/handleFlags-adp-method.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md),
[`plot,adp-method`](https://dankelley.github.io/oce/reference/plot-adp-method.md),
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md),
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md),
[`read.adp.nortek()`](https://dankelley.github.io/oce/reference/read.adp.nortek.md),
[`read.adp.rdi()`](https://dankelley.github.io/oce/reference/read.adp.rdi.md),
[`read.adp.sontek()`](https://dankelley.github.io/oce/reference/read.adp.sontek.md),
[`read.adp.sontek.serial()`](https://dankelley.github.io/oce/reference/read.adp.sontek.serial.md),
[`read.aquadopp()`](https://dankelley.github.io/oce/reference/read.aquadopp.md),
[`read.aquadoppHR()`](https://dankelley.github.io/oce/reference/read.aquadoppHR.md),
[`read.aquadoppProfiler()`](https://dankelley.github.io/oce/reference/read.aquadoppProfiler.md),
[`rotateAboutZ()`](https://dankelley.github.io/oce/reference/rotateAboutZ.md),
[`setFlags,adp-method`](https://dankelley.github.io/oce/reference/setFlags-adp-method.md),
[`subset,adp-method`](https://dankelley.github.io/oce/reference/subset-adp-method.md),
[`subtractBottomVelocity()`](https://dankelley.github.io/oce/reference/subtractBottomVelocity.md),
[`summary,adp-method`](https://dankelley.github.io/oce/reference/summary-adp-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdp()`](https://dankelley.github.io/oce/reference/toEnuAdp.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdp()`](https://dankelley.github.io/oce/reference/xyzToEnuAdp.md),
[`xyzToEnuAdpAD2CP()`](https://dankelley.github.io/oce/reference/xyzToEnuAdpAD2CP.md)

## Author

Jaimie Harbin and Dan Kelley

## Examples

``` r
library(oce)
data(adp)
adp[["a"]][, , 1][, 1]
#>  [1] 99 9d 9f 87 8a 99 9c 96 91 91 92 99 94 94 96 9c 98 a7 a2 94 92 8d 90 8f 90
ADP <- adpConvertRawToNumeric(adp)
ADP[["a"]][, , 1][, 1]
#>  [1] 153 157 159 135 138 153 156 150 145 145 146 153 148 148 150 156 152 167 162
#> [20] 148 146 141 144 143 144
```
