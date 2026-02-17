# Translate ODF CODE Strings to oce Variable Names

Translate ODF CODE strings to oce variable names. This is done
differently for data names and quality-control (QC) names.

## Usage

``` r
ODFNames2oceNames(
  ODFnames,
  columns = NULL,
  PARAMETER_HEADER = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- ODFnames:

  vector of character values that hold ODF names.

- columns:

  Optional list containing name correspondances, as described for
  [`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md).

- PARAMETER_HEADER:

  Optional list containing information on the data variables.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A list relating ODF names to oce names (see “Examples”).

## Details

The following table gives the recognized ODF code names for variables,
along with the translated names as used in oce objects. Note that the
code names are appended with strings such as `"_01"`, `"_02"`, etc, for
repeats. The converted name for an `"_01"` item is as shown below, and
for e.g. `"_02"` a suffix 2 is added to the oce name, etc.

QC items (which get stored as `flags` in object's `metadata` slots) are
assigned names that match those of the parameters to which they refer.
In parsing ODF files, it is assumed that QC items refer to the data
items that precede them. This pattern does not seem to be documented,
but it has held in all the files examined by the author, and a similar
assumption is made in other software systems. QC items have `CODE`
values that are either start with `"QQQQ"` or equal `"Q<CODE>"`, where
`<CODE>` matches the corresponding data item.

|  |  |  |
|----|----|----|
| **ODF Code** | **Oce Name** | **Notes** |
| `ABSH` | `humidityAbsolute` |  |
| `ACO2` | `CO2Atmosphere` |  |
| `ALKW` | `alkalinity` |  |
| `ALKY` | `alkalinityTotal` |  |
| `ALP0` | `apha0` |  |
| `ALTB` | `altimeter` |  |
| `ALTS` | `altitude` |  |
| `AMON` | `ammonium` |  |
| `ATMP` | `pressureAtmosphere` |  |
| `ATMS` | `pressureAtmosphereSealevel` |  |
| `ATRK` | `alongTrackDisplacement` |  |
| `ATTU` | `attenuation` |  |
| `AUTH` | `authority` |  |
| `BATH` | `barometricDepth` |  |
| `BATT` | `batteryVoltage` |  |
| `BEAM` | `a` |  |
| `BNO7` | `bestNODC7Number` | That is an "oh" letter, not a zero |
| `CALK` | `carbonateAlkalinity` |  |
| `CHLR` | `chlorinity` |  |
| `CHLS` | `chlorosity` |  |
| `CNDC` | `conductivity` |  |
| `CNTR` | `scan` |  |
| `COND` | `conductivity` |  |
| `CORG` | `carbonOrganic` |  |
| `CPHL` | `chlorophyll` |  |
| `CRAT` | `conductivity` | Conductivity ratio (may have spurious unit) |
| `CMNT` | `comment` |  |
| `CNDC` | `conductivity` |  |
| `COND` | `conductivity` |  |
| `CTOT` | `carbonTotal` |  |
| `DCHG` | `discharge` |  |
| `DENS` | `density` |  |
| `DEPH` | `pressure` |  |
| `DEWT` | `temperatureDewpoint` |  |
| `DOC_` | `carbonOrganicDissolved` |  |
| `DON_` | `nitrogenOrganicDissolved` |  |
| `DOXY` | `oxygen` |  |
| `DPDT` | `dpdt` |  |
| `DRDP` | `drogueDepth` |  |
| `DPWT` | `dryWeight` |  |
| `DRYT` | `temperatureDryBulb` |  |
| `DYNH` | `dynamicHeight` |  |
| `ERRV` | `errorVelocity` |  |
| `EWCM` | `uMagnetic` |  |
| `EWCT` | `u` |  |
| `FFFF` | `overall(FFFF)` | Archaic overall flag, replaced by `QCFF` |
| `FLOR` | `fluorometer` |  |
| `GDIR` | `windDirectionGust` |  |
| `GEOP` | `geopotential` |  |
| `GSPD` | `windSpeedGust` |  |
| `HCDM` | `directionMagnetic` |  |
| `HCDT` | `directionTrue` |  |
| `HCSP` | `speedHorizontal` |  |
| `HEAD` | `heading` |  |
| `HSUL` | `hydrogenSulphide` |  |
| `IDEN` | `sampleNumber` |  |
| `LABT` | `temperatureLaboratory` |  |
| `LATD` | `latitude` |  |
| `LHIS` | `lifeHistory` |  |
| `LOND` | `longitude` |  |
| `LPHT` | `pHLaboratory` |  |
| `MNSV` | `retentionFilterSize` |  |
| `MNSZ` | `organismSizeMinimum` |  |
| `MODF` | `additionalTaxonomicInformation` |  |
| `MXSZ` | `organismSizeMaximum` |  |
| `NETR` | `netSolarRadiation` |  |
| `NONE` | `noWMOcode` |  |
| `NORG` | `nitrogenOrganic` |  |
| `NSCM` | `vMagnetic` |  |
| `NSCT` | `v` |  |
| `NTOT` | `nitrogenTotal` |  |
| `NTRA` | `nitrate` |  |
| `NTRI` | `nitrite` |  |
| `NTRZ` | `nitrite+nitrate` |  |
| `NUM_` | `scansPerAverage` |  |
| `OBKS` | `turbidity` |  |
| `OCUR` | `oxygenCurrent` |  |
| `OPPR` | `oxygenPartialPressure` |  |
| `OSAT` | `oxygenSaturation` |  |
| `OTMP` | `oxygenTemperature` |  |
| `OXYG` | `oxygenDissolved` |  |
| `OXYM` | `oxygenDissolved` |  |
| `OXYV` | `oxygenVoltage` |  |
| `OXV_` | `oxygenVoltageRaw` |  |
| `PCO2` | `CO2` |  |
| `PHA_` | `phaeopigment` |  |
| `PHOS` | `phosphate` |  |
| `PHPH` | `pH` |  |
| `PHT_` | `pHTotal` |  |
| `PIM_` | `particulateInorganicMatter` |  |
| `PHY_` | `phytoplanktonCount` |  |
| `POC_` | `particulateOrganicCarbon` |  |
| `POM_` | `particulateOrganicMatter` |  |
| `PON_` | `particulateOrganicNitrogen` |  |
| `POTM` | `theta` |  |
| `PRES` | `pressure` |  |
| `PSAL` | `salinity` |  |
| `PSAR` | `PSAR` |  |
| `PTCH` | `pitch` |  |
| `QCFF` | `overall(QCFF)` | Overall flag (see also archaic FFFF) |
| `RANG` | `range` |  |
| `REFR` | `reference` |  |
| `RELH` | `humidityRelative` |  |
| `RELP` | `relativeTotalPressure` |  |
| `ROLL` | `roll` |  |
| `SDEV` | `standardDeviation` |  |
| `SECC` | `SecchiDepth` |  |
| `SEX_` | `sex` |  |
| `SIG0` | `sigma0` |  |
| `SIGP` | `sigmaTheta` |  |
| `SIGT` | `sigmat` |  |
| `SLCA` | `silicate` |  |
| `SNCN` | `scanCounter` |  |
| `SPAR` | `SPAR` |  |
| `SPEH` | `humiditySpecific` |  |
| `SPFR` | `sampleFraction` |  |
| `SPVO` | `specificVolume` |  |
| `SPVA` | `specificVolumeAnomaly` |  |
| `STRA` | `stressAmplitude` |  |
| `STRD` | `stressDirection` |  |
| `STRU` | `stressU` |  |
| `STRV` | `stressV` |  |
| `SSAL` | `salinity` |  |
| `SVEL` | `soundVelocity` |  |
| `SYTM` | `time` |  |
| `TAXN` | `taxonomicName` |  |
| `TE90` | `temperature` |  |
| `TEMP` | `temperature` |  |
| `TEXZT` | `text` |  |
| `TICW` | `totalInorganicCarbon` |  |
| `TILT` | `tilt` |  |
| `TOTP` | `pressureAbsolute` |  |
| `TPHS` | `phosphorousTotal` |  |
| `TRAN` | `lightTransmission` |  |
| `TRB_` | `turbidity` |  |
| `TRBH` | `trophicDescriptor` |  |
| `TSM_` | `suspendedMatterTotal` |  |
| `TSN_` | `taxonomicSerialNumber` |  |
| `TURB` | `turbidity` |  |
| `UNKN` | `-` |  |
| `UREA` | `urea` |  |
| `VAIS` | `BVFrequency` |  |
| `VCSP` | `w` |  |
| `VMXL` | `waveHeightMaximum` |  |
| `VRMS` | `waveHeightMean` |  |
| `VTCA` | `wavePeriod` |  |
| `WDIR` | `windDirection` |  |
| `WETT` | `temperatureWetBulb` |  |
| `WSPD` | `windSpeed` |  |
| `WTWT` | `wetWeight` |  |
| `ZOO_` | `zooplanktonCount` |  |

Any code not shown in the list is transferred to the oce object without
renaming, apart from the adjustment of suffix numbers. The following
code have been seen in data files from the Bedford Institute of
Oceanography: `ALTB`, `PHPH` and `QCFF`.

## References

For sources that describe the ODF format, see the documentation for the
[odf](https://dankelley.github.io/oce/reference/odf-class.md).

## See also

Other functions that interpret variable names and units from headers:
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`unitFromString()`](https://dankelley.github.io/oce/reference/unitFromString.md),
[`unitFromStringRsk()`](https://dankelley.github.io/oce/reference/unitFromStringRsk.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)

Other functions that convert variable names to the oce convention:
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`bodcNames2oceNames()`](https://dankelley.github.io/oce/reference/bodcNames2oceNames.md),
[`metNames2oceNames()`](https://dankelley.github.io/oce/reference/metNames2oceNames.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md)

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)

## Author

Dan Kelley

## Examples

``` r
ODFNames2oceNames("TEMP_01")$names # "temperature"
#> [1] "temperature"
```
