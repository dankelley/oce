# Convert adp Object From XYZ to ENU Coordinates

Convert ADP velocity components from a xyz-based coordinate system to an
enu-based coordinate system, by using the instrument's recording of
information relating to heading, pitch, and roll. The action is based on
what is stored in the data, and so it depends greatly on instrument type
and the style of original data format. This function handles data from
RDI Teledyne, Sontek, and some Nortek instruments directly.

## Usage

``` r
xyzToEnuAdp(x, declination = 0, debug = getOption("oceDebug"))
```

## Arguments

- x:

  an [adp](https://dankelley.github.io/oce/reference/adp-class.md)
  object.

- declination:

  magnetic declination to be added to the heading after "righting" (see
  below), to get ENU with N as "true" north. If this is set to NULL,
  then the returned object is set up without adjusting the compass for
  declination. That means that `north` in its `metadata` slot will be
  set to `"magnetic"`, and also that there will be no item named
  `declination` in that slot. Note that
  [`applyMagneticDeclination()`](https://dankelley.github.io/oce/reference/applyMagneticDeclination.md)
  can be used later, to set a declination.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

An object with `data$v[,,1:3]` altered appropriately, and
`x[["oceCoordinate"]]` changed from `xyz` to `enu`.

## Details

The first step is to convert the (x,y,z) velocity components (stored in
the three columns of `x[["v"]][,,1:3]`) into what RDI (reference 1,
pages 11 and 12) calls "ship" (or "righted") components. For example,
the z coordinate, which may point upwards or downwards depending on
instrument orientation, is mapped onto a "mast" coordinate that points
more nearly upwards than downward. The other ship coordinates are called
"starboard" and "forward", the meanings of which will be clear to
mariners. Once the (x,y,z) velocities are converted to ship velocities,
the orientation of the instrument is extracted from heading, pitch, and
roll vectors stored in the object. These angles are defined differently
for RDI and Sontek profilers.

The code handles every case individually, based on the table given
below. The table comes from Clark Richards, a former PhD student at
Dalhousie University (reference 2), who developed it based on instrument
documentation, discussion on user groups, and analysis of measurements
acquired with RDI and Sontek acoustic current profilers in the SLEIWEX
experiment. In the table, (X, Y, Z) denote instrument-coordinate
velocities, (S, F, M) denote ship-coordinate velocities, and (H, P, R)
denote heading, pitch, and roll.

|  |  |  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|----|
| **Case** | **Mfr.** | **Instr.** | **Orient.** | **H** | **P** | **R** | **S** | **F** | **M** | 1 | RDI |
| ADCP | up | H | arctan(tan(P)\*cos(R)) | R | -X | Y | -Z | 2 | RDI | ADCP | down |
| H | arctan(tan(P)\*cos(R)) | -R | X | Y | Z | 3 | Nortek | ADP | up | H-90 | R |
| -P | X | Y | Z | 4 | Nortek | ADP | down | H-90 | R | -P | X |
| -Y | -Z | 5 | Sontek | ADP | up | H-90 | -P | -R | X | Y | Z |
| 6 | Sontek | ADP | down | H-90 | -P | -R | X | Y | Z | 7 | Sontek |
| PCADP | up | H-90 | R | -P | X | Y | Z | 8 | Sontek | PCADP | down |

Finally, a standardized rotation matrix is used to convert from ship
coordinates to earth coordinates (see pages 13 and 14 of the RDI
coordinate transformation manual, reference 1).

## References

1.  Teledyne RD Instruments. “ADCP Coordinate Transformation: Formulas
    and Calculations,” January 2010. P/N 951-6079-00.

2.  Clark Richards, 2012, PhD Dalhousie University Department of
    Oceanography.

## See also

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
[`xyzToEnuAdpAD2CP()`](https://dankelley.github.io/oce/reference/xyzToEnuAdpAD2CP.md)

## Author

Dan Kelley and Clark Richards
