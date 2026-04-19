# Map AD2CP ID Code to oce Name

As explained in Nortek (2022, section 6.1, page 80), AD2CP files use a
hexadecimal (in R, "raw") code to indicate the nature of each data
chunk, and
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md)
uses the present function as it analyses AD2CP files.

## Usage

``` r
ad2cpCodeToName(code = NULL, prefix = TRUE)
```

## Arguments

- code:

  a [raw](https://rdrr.io/r/base/raw.html) (or corresponding integer)
  vector indicating the IDs of interest, or NULL to get a summary of
  possible values.

- prefix:

  logical value indicating whether to show the raw value as a prefix
  (e.g. `"0x1c=echosounder"` as opposed to `"echosounder"`).

## Value

An indication of the mapping. If `code` is NULL, this is a data frame.
Otherwise, it is a character vector with the relevant mappings, with the
raw form of the code linked with the name, as in the example.

## Details

The mapping from code (hex or decimal) to oce name is as follows.

|            |                |                       |
|------------|----------------|-----------------------|
| code (raw) | code (integer) | oce name              |
| ———-       | ————–          | —————–                |
| `0x15`     | 21             | `burst`               |
| `0x16`     | 22             | `average`             |
| `0x17`     | 23             | `bottomTrack`         |
| `0x18`     | 24             | `interleavedBurst`    |
| `0x1a`     | 26             | `burstAltimeterRaw`   |
| `0x1b`     | 27             | `DVLBottomTrack`      |
| `0x1c`     | 28             | `echosounder`         |
| `0x1d`     | 29             | `DVLWaterTrack`       |
| `0x1e`     | 30             | `altimeter`           |
| `0x1f`     | 31             | `averageAltimeterRaw` |
| `0x23`     | 35             | `echosounderRaw`      |
| `0x24`     | 36             | `echosounderRawTx`    |
| `0x30`     | 48             | `waves`               |
| `0xa0`     | 160            | `text`                |

## References

Nortek AS. “Signature Integration 55\|250\|500\|1000kHz.” Nortek AS,
March 31, 2022.

## See also

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
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

Other things related to ad2cp data:
[`ad2cpHeaderValue()`](https://dankelley.github.io/oce/reference/ad2cpHeaderValue.md),
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`is.ad2cp()`](https://dankelley.github.io/oce/reference/is.ad2cp.md),
[`read.adp.ad2cp()`](https://dankelley.github.io/oce/reference/read.adp.ad2cp.md)

## Author

Dan Kelley

## Examples

``` r
stopifnot(ad2cpCodeToName(0x15) == "0x15=burst")
```
