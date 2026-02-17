# Trim a Sontek ADR adv File

Create a Sontek ADR adv (acoustic Doppler velocimeter) file by copying
the header plus the first `n` data chunks (recognized by the three-byte
sequence `0xA5`, `0x11`, \`0x3c') into a new file. This can be useful in
supplying small sample files for bug reports.

## Usage

``` r
advSontekAdrFileTrim(infile, n = 100, outfile, debug = getOption("oceDebug"))
```

## Arguments

- infile:

  name of a Sontek ADR adp file.

- n:

  integer indicating the number of data chunks to keep. The default is
  to keep 100 chunks, a common choice for sample files.

- outfile:

  optional name of the new Sontek ADR adp file to be created. If this is
  not supplied, a default is used, by adding `_trimmed` to the base
  filename, e.g. if `infile` is `"x.adr"` then `outfile` will be
  `x_trimmed.adr`.

- debug:

  an integer value indicating the level of debugging. If this is 1L,
  then a brief indication is given of the processing steps. If it is \>
  1L, then information is given about each data chunk, which can yield
  very extensive output.

## Value

`advSontekAdrFileTrim()` returns the name of the output file, `outfile`,
as provided or constructed.

## See also

Other things related to adv data:
[`[[,adv-method`](https://dankelley.github.io/oce/reference/sub-sub-adv-method.md),
`[[<-,adv-method`,
[`adv`](https://dankelley.github.io/oce/reference/adv.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
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

Other functions that trim data files:
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`oceFileTrim()`](https://dankelley.github.io/oce/reference/oceFileTrim.md)
