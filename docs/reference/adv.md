# Sample adv Data

This [adv](https://dankelley.github.io/oce/reference/adv-class.md)
object is a sampling of measurements made with a Nortek Vector acoustic
Doppler velocimeter deployed as part of the St Lawrence Internal Wave
Experiment (SLEIWEX). Various identifying features have been redacted.

## Usage

``` r
data(adv)
```

## Source

This file came from the SLEIWEX-2008 experiment.

## See also

Other datasets provided with oce:
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`cm`](https://dankelley.github.io/oce/reference/cm.md),
[`coastlineWorld`](https://dankelley.github.io/oce/reference/coastlineWorld.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`landsat`](https://dankelley.github.io/oce/reference/landsat.md),
[`lisst`](https://dankelley.github.io/oce/reference/lisst.md),
[`lobo`](https://dankelley.github.io/oce/reference/lobo.md),
[`met`](https://dankelley.github.io/oce/reference/met.md),
[`ocecolors`](https://dankelley.github.io/oce/reference/ocecolors.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`section`](https://dankelley.github.io/oce/reference/section.md),
[`topoWorld`](https://dankelley.github.io/oce/reference/topoWorld.md),
[`wind`](https://dankelley.github.io/oce/reference/wind.md),
[`xbt`](https://dankelley.github.io/oce/reference/xbt.md)

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
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
[`summary,adv-method`](https://dankelley.github.io/oce/reference/summary-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md),
[`xyzToEnuAdv()`](https://dankelley.github.io/oce/reference/xyzToEnuAdv.md)

## Examples

``` r
# \donttest{
library(oce)
data(adv)

# Velocity time-series
plot(adv)


# Spectrum of upward component of velocity, with ``turbulent'' reference line
s <- spectrum(adv[["v"]][, 3], plot = FALSE)
plot(log10(s$freq), log10(s$spec), type = "l")
for (a in seq(-20, 20, by = 1)) {
    abline(a = a, b = -5 / 3, col = "gray", lty = "dotted")
}

# }
```
