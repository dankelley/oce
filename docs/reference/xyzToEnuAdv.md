# Convert an adv Object From XYZ to ENU Coordinates

Convert ADV velocity components from a xyz-based coordinate system to an
enu-based coordinate system.

## Usage

``` r
xyzToEnuAdv(
  x,
  declination = 0,
  cabled = FALSE,
  horizontalCase,
  sensorOrientation,
  debug = getOption("oceDebug")
)
```

## Arguments

- x:

  an [adv](https://dankelley.github.io/oce/reference/adv-class.md)
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

- cabled:

  boolean value indicating whether the sensor head is connected to the
  pressure case with a cable. If `cabled=FALSE`, then `horizontalCase`
  is ignored. See “Details”.

- horizontalCase:

  optional boolean value indicating whether the sensor case is oriented
  horizontally. Ignored unless `cabled` is `TRUE`. See “Details”.

- sensorOrientation:

  optional string indicating the direction in which the sensor points.
  The value, which must be `"upward"` or `"downward"`, over-rides the
  value of `orientation`, in the `metadata` slot, which will have been
  set by
  [`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md),
  *provided* that the data file contained the full header. See
  “Details”.

- debug:

  a flag that, if non-zero, turns on debugging. Higher values yield more
  extensive debugging.

## Details

The coordinate transformation is done using the heading, pitch, and roll
information contained within `x`. The algorithm is similar to that used
for Teledyne/RDI ADCP units, taking into account the different
definitions of heading, pitch, and roll as they are defined for the
velocimeters.

Generally, the transformation must be done on a time-by-time basis,
which is a slow operation. However, this function checks whether the
vectors for heading, pitch and roll, are all of unit length, and in that
case, the calculation is altered, resulting in shorter execution times.
Note that the angles are held in (`data$timeSlow`, `data$headingSlow`,
...) for Nortek instruments and (`data$time`, `data$heading`, ...) for
Sontek instruments.

Since the documentation provided by instrument manufacturers can be
vague on the coordinate transformations, the method used here had to be
developed indirectly. (This is in contrast to the RDI ADCP instruments,
for which there are clear instructions.) documents that manufacturers
provide. If results seem incorrect (e.g. if currents go east instead of
west), users should examine the code in detail for the case at hand. The
first step is to set `debug` to 1, so that the processing will print a
trail of processing steps. The next step should be to consult the table
below, to see if it matches the understanding (or empirical tests) of
the user. It should not be difficult to tailor the code, if needed.

The code handles every case individually, based on the table given
below. The table comes from Clark Richards, a former PhD student at
Dalhousie University (reference 2), who developed it based on instrument
documentation, discussion on user groups, and analysis of measurements
acquired with Nortek and Sontek velocimeters in the SLEIWEX experiment.

The column labelled
`Cabled'' indicates whether the sensor and the pressure case are connected with a flexible cable, and the column labelled `H.case”
indicates whether the pressure case is oriented horizontally. These two
properties are not discoverable in the headers of the data files, and so
they must be supplied with the arguments `cabled` and `horizontalCase`.
The source code refers to the information in this table by case numbers.
(Cases 5 and 6 are not handled.) Angles are abbreviated as follows::
heading `H,'' pitch `P,” and roll “R”. Entries X, Y and Z refer to
instrument coordinates of the same names. Entries S, F and M refer to
so-called ship coordinates starboard, forward, and mast; it is these
that are used together with a rotation matrix to get velocity components
in the east, north, and upward directions.

|  |  |  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|----|
| **`Case`** | **`Mfr.`** | **`Instr.`** | **`Cabled`** | **`H. case`** | **`Orient.`** | **`H`** | **`P`** | **`R`** | **`S`** | **`F`** | **`M`** |
| 1 | Nortek | vector | no | \- | up | H-90 | R | -P | X | -Y | -Z |
| 2 | Nortek | vector | no | \- | down | H-90 | R | -P | X | Y | Z |
| 3 | Nortek | vector | yes | yes | up | H-90 | R | -P | X | Y | Z |
| 4 | Nortek | vector | yes | yes | down | H-90 | R | P | X | -Y | -Z |
| 5 | Nortek | vector | yes | no | up | \- | \- | \- | \- | \- | \- |
| 6 | Nortek | vector | yes | no | down | \- | \- | \- | \- | \- | \- |
| 7 | Sontek | adv | \- | \- | up | H-90 | R | -P | X | -Y | -Z |
| 8 | Sontek | adv | \- | \- | down | H-90 | R | -P | X | Y | Z |

## References

1.  `https://nortek.zendesk.com/hc/en-us/articles/360029820971-How-is-a-Coordinate-transformation-done-`

2.  Clark Richards, 2012, PhD Dalhousie University Department of
    Oceanography.

## See also

See
[`read.adv()`](https://dankelley.github.io/oce/reference/read.adv.md)
for notes on functions relating to `adv` objects.

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
[`summary,adv-method`](https://dankelley.github.io/oce/reference/summary-adv-method.md),
[`toEnu()`](https://dankelley.github.io/oce/reference/toEnu.md),
[`toEnuAdv()`](https://dankelley.github.io/oce/reference/toEnuAdv.md),
[`velocityStatistics()`](https://dankelley.github.io/oce/reference/velocityStatistics.md),
[`xyzToEnu()`](https://dankelley.github.io/oce/reference/xyzToEnu.md)

## Author

Dan Kelley, in collaboration with Clark Richards
