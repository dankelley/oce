# Convert adp Object From Beam to XYZ Coordinates

Convert ADP velocity components from a beam-based coordinate system to a
xyz-based coordinate system. The action depends on the type of object.
Objects creating by reading RDI Teledyne, Sontek, and some Nortek
instruments are handled directly.

## Usage

``` r
beamToXyzAdp(x, debug = getOption("oceDebug"))
```

## Arguments

- x:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

An object with the first 3 velocity indices having been altered to
represent velocity components in xyz (or instrument) coordinates. (For
`rdi` data, the values at the 4th velocity index are changed to
represent the "error" velocity.) To indicate the change, the value of
`x[["oceCoordinate"]]` is changed from `beam` to `xyz`.

## Details

For a 3-beam Nortek `aquadopp` object, the beams are transformed into
velocities using the matrix stored in the header.

For 4-beam objects (and for the slanted 4 beams of 5-beam objects), the
along-beam velocity components \\B_1\\ \\B_2\\, \\B_3\\, and \\B_4\\ are
converted to Cartesian velocity components \\u\\ \\v\\ and \\w\\ using
formulae from section 5.5 of *RD Instruments* (1998), viz. the
along-beam velocity components \\B_1\\, \\B_2\\, \\B_3\\, and \\B_4\\
are used to calculate velocity components in a cartesian system
referenced to the instrument using the following formulae:
\\u=ca(B_1-B_2)\\, \\v=ca(B_4-B_3)\\, \\w=-b(B_1+B_2+B_3+B_4)\\. In
addition to these, an estimate of the error in velocity is computed as
\\e=d(B_1+B_2-B_3-B_4)\\. The geometrical factors in these formulae are:
`c` is +1 for convex beam geometry or -1 for concave beam geometry,
\\a=1/(2\sin\theta)\\ where \\\theta\\ is the angle the beams make to
the axial direction (which is available as `x[["beamAngle"]]`),
\\b=1/(4\cos\theta)\\, and \\d=a/\sqrt{2}\\.

## References

1.  Teledyne RD Instruments. “ADCP Coordinate Transformation: Formulas
    and Calculations,” January 2010. P/N 951-6079-00.

2.  WHOI/USGS-provided Matlab code for beam-enu transformation
    `http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m`

## See also

See
[`read.adp()`](https://dankelley.github.io/oce/reference/read.adp.md)
for other functions that relate to objects of class `"adp"`.

Other things related to adp data:
[`[[,adp-method`](https://dankelley.github.io/oce/reference/sub-sub-adp-method.md),
`[[<-,adp-method`,
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
