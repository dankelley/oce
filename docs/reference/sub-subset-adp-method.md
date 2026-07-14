# Replace Parts of an adp Object

In addition to the usual insertion of elements by name, note that e.g.
`pitch` gets stored into `pitchSlow`.

The `[[<-` method works for all
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects.
The purpose, as with the related extraction method, `[[`, is to insulate
users from the internal details of
[oce](https://dankelley.github.io/oce/reference/oce-class.md) objects,
by looking for items within the various storage slots of the object.
Items not actually stored can also be replaced, including units and
data-quality flags.

## Usage

``` r
# S4 method for class 'adp'
x[[i, j, ...]] <- value
```

## Arguments

- x:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- i:

  character value naming the item to replace.

- j:

  optional additional information on the `i` item.

- ...:

  optional additional information (ignored).

- value:

  The value to be placed into `x`, somewhere.

## Details

As with `[[` method, the procedure works in steps.

First, the `metadata` slot of `x` is checked to see whether it contains
something named with `i`. If so, then the named item is replaced with
`value`.

Otherwise, if the string value of `i` ends in `Unit`, then the
characters preceding that are taken as the name of a variable, and the
`metadata` slot of `x` is updated to store that unit, e.g.

    x[["temperatureUnits"]] <- list(unit=expression(degree*F),scale="")

Similarly, if `i` ends in `Flag`, then quality-control flags are set up
as defined by `result`, e.g.

    o[["temperatureFlags"]] <- c(2,4,2,2)

Otherwise, [`pmatch()`](https://rdrr.io/r/base/pmatch.html) is used for
a partial-string match with the names of the items that are in the
`data` slot of `x`. The first item found (if any) is then updated to
hold the value `result`.

If none of these conditions is met, a warning is issued.

## See also

Other functions that replace parts of oce objects: `[[<-,amsr-method`,
`[[<-,argo-method`, `[[<-,bremen-method`, `[[<-,cm-method`,
`[[<-,coastline-method`, `[[<-,ctd-method`, `[[<-,echosounder-method`,
`[[<-,g1sst-method`, `[[<-,gps-method`, `[[<-,ladp-method`,
`[[<-,landsat-method`, `[[<-,lisst-method`, `[[<-,lobo-method`,
`[[<-,met-method`, `[[<-,oce-method`, `[[<-,odf-method`,
`[[<-,rsk-method`, `[[<-,sealevel-method`, `[[<-,section-method`,
`[[<-,tidem-method`, `[[<-,topo-method`, `[[<-,windrose-method`,
`[[<-,xbt-method`

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
[`ad2cpCodeToName()`](https://dankelley.github.io/oce/reference/ad2cpCodeToName.md),
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adp`](https://dankelley.github.io/oce/reference/adp.md),
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpConvertRawToNumeric()`](https://dankelley.github.io/oce/reference/adpConvertRawToNumeric.md),
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

Dan Kelley
