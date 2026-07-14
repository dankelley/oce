# Convert Argo Data Name to Oce Name

This function is used internally by
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md)
to convert Argo-convention data names to oce-convention names. Users
should not call this directly, since its return value may be changed at
any moment (e.g. to include units as well as names).

## Usage

``` r
argoNames2oceNames(names, ignore.case = TRUE)
```

## Arguments

- names:

  vector of character strings containing names in the Argo convention.

- ignore.case:

  a logical value passed to
  [`gsub()`](https://rdrr.io/r/base/grep.html), indicating whether to
  ignore the case of input strings. The default is set to `TRUE` because
  some data files use lower-case names, despite the fact that the Argo
  documentation specifies upper-case.

## Value

A character vector of the same length as `names`, but with replacements
having been made for all known quantities.

## Details

Initially, Feb 2016, the inference of names was initially done by an
inspection of some data files, based on reference 1. Later, in June
2023, broader inspection of more files and documents yielded about ten
additions, and a single correction: `VRSpH` was renamed
`phSensorVoltageDifference`, to match related names that had been added.

It should be noted that the data files examined contain some names that
are not documented in reference 1, and others that are listed only in
its changelog, with no actual definitions being given. For example, the
files had six distinct variable names that seem to relate to phase in
the oxygen sensor, but these are not translated by the present function
because these variable names are not defined in reference 1, or not
defined uniquely in reference 2.

The names are converted with
[`gsub()`](https://rdrr.io/r/base/grep.html), using the `ignore.case`
argument of the present function. The procedure is to first handle the
items listed in the following table, with string searches anchored to
the start of the string. After that, the qualifiers `_ADJUSTED`,
`_ERROR` and `_QC`, are translated to `Adjusted`, `Error`, and `QC`,
respectively.

An incomplete list of name translations is as follows, where `~`
represents digit sequences in some instances and letters in others. Note
that until June 2023, `pHSensorVoltageDifference` was called `VRSpH`.

|  |  |
|----|----|
| **Argo name** | **oce name** |
| `BBP` | `bbp` |
| `BETA_BACKSCATTERING` | `betaBackscattering` |
| `BPHASE_OXY` | `bphaseOxygen` |
| `C~PHASE_DOXY` | `C~phaseOxygen` |
| `CDOM` | `CDOM` |
| `CNDC` | `conductivity` |
| `CHLA` | `chlorophyllA` |
| `CP` | `beamAttenuation` |
| `CYCLE_NUMBER` | `cycleNumber` (both this and `cycle` are handled by the \[\[ operator) |
| `DATA_CENTRE` | `dataCentre` |
| `DATA_MODE` | `dataMode` |
| `DATA_STATE_INDICATOR` | `dataStateIndicator` |
| `DC_REFERENCE` | `DCReference` |
| `DIRECTION` | `direction` |
| `DOWN_IRRADIANCE` | `downwellingIrradiance` |
| `DOWNWELLING_PAR` | `downwellingPAR` |
| `FIRMWARE_VERSION` | `firmwareVersion` |
| `FIT_ERROR_NITRATE` | `fitErrorNitrate` |
| `FLUORESCENCE_CDOM` | `fluorescenceCDOM` |
| `FLUORESCENCE_CHLA` | `fluorescenceChlorophyllA` |
| `IB_PH` | `pHBaseCurrent` |
| `IK_PH` | `pHCounterCurrent` |
| `INST_REFERENCE` | `instReference` |
| `JULD` | `juld` (and used to compute `time`) |
| `JULD_QC_LOCATION` | `juldQCLocation` |
| `LATITUDE` | `latitude` |
| `LONGITUDE` | `longitude` |
| `MOLAR_DOXY` | `oxygenUncompensated` |
| `MTIME` | `mtime` |
| `NB_SAMPLE_CTD` | `nbSampleCtd` |
| `PH_IN_SITU_FREE` | `pHFree` |
| `PH_IN_SITU_TOTAL` | `pH` |
| `PI_NAME` | `PIName` |
| `PLATFORM_NUMBER` | `id` |
| `POSITION_ACCURACY` | `positionAccuracy` |
| `POSITIONING_SYSTEM` | `positioningSystem` |
| `PROFILE` | `profile` |
| `PROJECT_NAME` | `projectName` |
| `RAW_DOWNWELLING_IRRADIANCE` | `rawDownwellingIrradiance` |
| `RAW_DOWNWELLING_PAR` | `rawDownwellingPAR` |
| `RAW_UPWELLING_RADIANCE` | `rawUpwellingRadiance` |
| `STATION_PARAMETERS` | `stationParameters` |
| `TEMP` | `temperature` |
| `TEMP_CPU_CHLA` | `temperatureCPUChlorophyllA` |
| `TEMP_DOXY` | `temperatureOxygen` |
| `TEMP_NITRATE` | `temperatureNitrate` |
| `TEMP_PH` | `temperaturePH` |
| `TEMP_SPECTROPHOTOMETER_NITRATE` | `temperatureSpectrophotometerNitrate` |
| `TILT` | `tilt` |
| `TPHASE_DOXY` | `tphaseOxygen` |
| `TURBIDITY` | `turbidity` |
| `UP_RADIANCE` | `upwellingRadiance` |
| `UV_INTENSITY` | `UVIntensity` |
| `UV_INTENSITY_DARK_NITRATE` | `UVIntensityDarkNitrate` |
| `UV_INTENSITY_NITRATE` | `UVIntensityNitrate` |
| `VRS_PH` | `pHSensorVoltageDifference` |
| `WMO_INST_TYPE` | `WMOInstType` |

## References

1.  Argo User's Manual Version 3.3, Nov 89th, 2019, available at
    `https://archimer.ifremer.fr/doc/00187/29825/` online.

2.  Argo list of parameters in an excel spreadsheet, available at
    `http://www.argodatamgt.org/content/download/27444/187206/file/argo-parameters-list-core-and-b.xlsx`

## See also

Other functions that convert variable names to the oce convention:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`bodcNames2oceNames()`](https://dankelley.github.io/oce/reference/bodcNames2oceNames.md),
[`metNames2oceNames()`](https://dankelley.github.io/oce/reference/metNames2oceNames.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md)

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo()`](https://dankelley.github.io/oce/reference/read.argo.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley, with help from Anna Victor
