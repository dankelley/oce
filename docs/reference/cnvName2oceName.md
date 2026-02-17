# Infer Variable Name, Units and Scale From a Seabird Header

This function is used by
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
to infer data names and units from the coding used by Teledyne/Seabird
(SBE) `.cnv` files. Lacking access to documentation on the SBE format,
the present function is based on inspection of a suite of CNV files
available to the `oce` developers.

## Usage

``` r
cnvName2oceName(
  h,
  columns = NULL,
  newNameFormat = FALSE,
  debug = getOption("oceDebug")
)
```

## Arguments

- h:

  character value holding the header line.

- columns:

  optional list containing name correspondences, as described for
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md).

- newNameFormat:

  logical value indicating whether the column names in the header are in
  the traditional format (with an abbreviated name followed by a colon
  and then more information) or a new format noticed in a file provided
  with a github issue (https://github.com/dankelley/oce/issues/2328).
  Until (or unless) SBE provides information on this new format, the
  present parameter is provisional, as is all code relating to reading
  new-format files. (The file is produced by the new SBE software called
  Fathom.)

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

A few sample header lines that have been encountered are:

    # name 4 = t068: temperature, IPTS-68 [deg C]
    # name 3 = t090C: Temperature [ITS-90, deg C]
    # name 4 = t190C: Temperature, 2 [ITS-90, deg C]

Examination of several CNV files suggests that it is best to try to
infer the name from the characters between the "`=`" and "`:`"
characters, because the material after the colon seems to vary more
between sample files.

The table given below indicates the translation patterns used. These are
taken from reference 1. The `.cnv` convention for multiple sensors is to
include optional extra digits in the name, and these are indicated with
one `#` character for each optional digit. (These `#` characters are
converted to the the appropriate regular-expression code, `\\d`, before
pattern-matching with [`grep()`](https://rdrr.io/r/base/grep.html).)

It is important to note that this table is by no means complete, since
there are a great many SBE names listed in their document (reference 1),
plus names not listed there but present in data files supplied by
prominent archiving agencies. If an SBE name is not recognized, then the
oce name is set to that SBE name. This can cause problems in some other
processing steps (e.g. if
[`swRho()`](https://dankelley.github.io/oce/reference/swRho.md) or a
similar function is called with an `oce` object as first argument), and
so users are well-advised to rename the items as appropriate. The first
step in doing this is to pass the object to
[`summary()`](https://rdrr.io/r/base/summary.html), to discover the SBE
names in question. Then consult the SBE documentation to find an
appropriate name for the data, and either manipulate the names in the
object data slot directly or use
[`oceRename()`](https://dankelley.github.io/oce/reference/oceRename.md)
to rename the elements. Finally, please publish an 'issue' on the oce
Github site `https://github.com/dankelley/oce/issues` so that the
developers can add the data type in question.

The table below has well over 100 entries, but this only scratches the
surface; the SBE documents list over 40 variants for oxygen alone. To
save development time, there is no plan to add all possible data types
without a reasonable and specific expression user interest.

|  |  |  |  |
|----|----|----|----|
| **Key** | **Result** | **Unit;scale** | **Notes** |
| `accM` | `acceleration` | m/s^2 |  |
| `altM` | `altimeter` | m |  |
| `alt` | `altimeter` | m |  |
| `bat#` | `beamAttenuation` | 1/m |  |
| `C2-C1mS/cm` | `conductivityDifference` | mS/cm |  |
| `C2-C1S/m` | `conductivityDifference` | S/m |  |
| `C2-C1uS/cm` | `conductivityDifference` | uS/cm |  |
| `cond#mS/cm` | `conductivity` | mS/cm |  |
| `cond#S/m` | `conductivity` | S/m |  |
| `cond#uS/cm` | `conductivity` | uS/cm |  |
| `CStarAt#` | `beamAttenuation` | 1/m |  |
| `CStarTr#` | `beamTransmission` | percent |  |
| `c#mS/cm` | `conductivity` | mS/cm |  |
| `c#S/m` | `conductivity` | S/m |  |
| `c#uS/cm` | `conductivity` | uS/cm |  |
| `density##` | `density` | kg/m^3 |  |
| `depFM` | `depth` | m |  |
| `depF` | `depth` | m |  |
| `depSM` | `depth` | m |  |
| `depS` | `depth` | m |  |
| `dz/dtM` | `descentRate` | m/s |  |
| `flCM` | `fluorescence` | ug/l; Chelsea Mini Chl Con |  |
| `flCUVA#` | `fluorescence` | ug/l; Chelsea UV Aquatracka |  |
| `flC#` | `fluorescence` | ug/l; Chelsea Aqua 3 |  |
| `flEC-AFL#` | `fluorescence` | mg/m^3; WET Labs ECO-AFL/FL |  |
| `flScufa#` | `fluorescence` | -; Turner SCUFA (RFU) |  |
| `flSPR` | `fluorescence` | -; Seapoint, Rhodamine |  |
| `flSPuv` | `fluorescence` | -; Seapoint, UV |  |
| `flSP` | `fluorescence` | -; Seapoint |  |
| `flS` | `fluorescence` | -; Seatech |  |
| `flT` | `fluorescence` | -; Turner 10-005 flT |  |
| `f#` | `frequency` | Hz |  |
| `f##` | `frequency` | Hz |  |
| `gpa` | `geopotentialAnomaly` | -; J/kg |  |
| `latitude` | `latitude` | degN |  |
| `longitude` | `longitude` | degE |  |
| `n2satMg/L` | `nitrogenSaturation` | mg/l |  |
| `n2satML/L` | `nitrogenSaturation` | ml/l |  |
| `n2satumol/kg` | `nitrogenSaturation` | umol/kg |  |
| `nbin` | `nbin` |  |  |
| `obsscufa#` | `backscatter` | NTU; Turner SCUFA |  |
| `opoxMg/L` | `oxygen` | mg/l; Optode, Aanderaa |  |
| `opoxML/L` | `oxygen` | ml/l; Optode, Aanderaa |  |
| `opoxMm/L` | `oxygen` | umol/l; Optode, Aanderaa |  |
| `opoxPS` | `oxygen` | percent; Optode, Aanderaa |  |
| `oxsatMg/L` | `oxygen` | mg/l; Weiss |  |
| `oxsatML/L` | `oxygen` | ml/l; Weiss |  |
| `oxsatMm/Kg` | `oxygen` | umol/kg; Weiss |  |
| `oxsolMg/L` | `oxygen` | mg/l; Garcia-Gordon |  |
| `oxsolML/L` | `oxygen` | ml/l; Garcia-Gordon |  |
| `oxsolMm/Kg` | `oxygen` | umol/kg; Garcia-Gordon |  |
| `par/log` | `PAR` | log; Satlantic |  |
| `par#` | `PAR` | -; Biospherical/Licor |  |
| `ph` | `pH` | \- |  |
| `potemp#68C` | `thetaM` | degC; IPTS-68 |  |
| `potemp#90C` | `thetaM` | degC; ITS-90 |  |
| `pr50M` | `pressure` | dbar; SBE50 |  |
| `prDE` | `pressure` | psi; digiquartz | 2 |
| `prdE` | `pressure` | psi; strain gauge | 2 |
| `prDM` | `pressure` | dbar; digiquartz |  |
| `prdM` | `pressure` | dbar; strain gauge |  |
| `prM` | `pressure` | dbar |  |
| `prSM` | `pressure` | dbar |  |
| `prSM` | `pressure` | dbar; strain gauge |  |
| `pr` | `pressure` | dbar | 1 |
| `ptempC` | `pressureTemperature` | degC; ITS-90 | 3 |
| `pumps` | `pumpStatus` |  |  |
| `rhodflTC#` | `Rhodamine` | ppb; Turner Cyclops |  |
| `sal##` | `salinity` | -, PSS-78 | 4 |
| `sbeox#ML/L` | `oxygen` | ml/l; SBE43 |  |
| `sbeox#Mm/Kg` | `oxygen` | umol/kg; SBE43 |  |
| `sbeox#Mm/L` | `oxygen` | umol/l; SBE43 |  |
| `sbeox#PS` | `oxygen` | percent; SBE43 |  |
| `sbeox#V` | `oxygenRaw` | V; SBE43 |  |
| `sbox#dV/dT` | `oxygen` | dov/dt; SBE43 |  |
| `sbox#ML/L` | `oxygen` | ml/l; SBE43 |  |
| `sbox#Mm/Kg` | `oxygen` | umol/kg; SBE43 |  |
| `sbox#Mm/L` | `oxygen` | umol/l; SBE43 |  |
| `sbox#PS` | `oxygen` | percent; SBE43 |  |
| `sbox#V` | `oxygenRaw` | V; SBE43 |  |
| `scan` | `scan` | \- |  |
| `seaTurbMtr#` | `turbidity` | FTU; Seapoint |  |
| `secS-priS` | `salinityDifference` | -, PSS-78 |  |
| `sigma-`é | `sigmaTheta` | kg/m^3 | 5 |
| `sigma-t` | `sigmaT` | kg/m^3 |  |
| `sigma-theta` | `sigmaTheta` | kg/m^3 | 5 |
| `spar` | `spar` | \- |  |
| `specc` | `specificConductance` | uS/cm |  |
| `sva` | `specificVolumeAnomaly` | 1e-8 m^3/kg; |  |
| `svCM#` | `soundSpeed` | m/s; Chen-Millero |  |
| `t090Cm` | `temperature` | degC; ITS-90 |  |
| `t190C` | `temperature` | degC; ITS-90 |  |
| `T2#68C` | `temperatureDifference` | degC; IPTS-68 |  |
| `T2#90C` | `temperatureDifference` | degC; ITS-90 |  |
| `t3868C#` | `temperature` | degC; IPTS-68 |  |
| `t3890C#` | `temperature` | degC; ITS-90 |  |
| `t38#38C` | `temperature` | degC; IPTS-68 |  |
| `t38#90C` | `temperature` | degC; ITS-90 |  |
| `t4968C` | `temperature` | degC; IPTS-68 |  |
| `t4990C` | `temperature` | degC; ITS-90 |  |
| `timeH` | `timeH` | hour; elapsed |  |
| `timeJV2` | `timeJV2` | julian day |  |
| `timeJ` | `timeJ` | julian day |  |
| `timeK` | `timeK` | s; since Jan 1, 2000 |  |
| `timeM` | `timeM` | minute; elapsed |  |
| `timeN` | `timeN` | s; NMEA since Jan 1, 1970 |  |
| `timeQ` | `timeQ` | s; NMEA since Jan 1, 2000 |  |
| `timeS` | `timeS` | s; elapsed |  |
| `tnc268C` | `temperature` | degC; IPTS-68 |  |
| `tnc290C` | `temperature` | degC; ITS-90 |  |
| `tnc68C` | `temperature` | degC; IPTS-68 |  |
| `tnc90C` | `temperature` | degC; ITS-90 |  |
| `tsa` | `thermostericAnomaly` | 1e-8 m^3/kg |  |
| `turbflTCdiff` | `turbidityDifference` | NTU; Turner Cyclops |  |
| `turbflTC#` | `turbidity` | NTU; Turner Cyclops |  |
| `turbWETbbdiff` | `turbidityDifference` | 1/(m\\sr); WET Labs ECO |  |
| `turbWETbb#` | `turbidity` | 1/(m\\sr); WET Labs ECO |  |
| `turbWETntudiff` | `turbidityDifference` | NTU; WET Labs ECO |  |
| `turbWETntu#` | `turbidity` | NTU; WET Labs ECO |  |
| `tv268C` | `temperature` | degC; IPTS-68 |  |
| `tv290C` | `temperature` | degC; ITS-90 |  |
| `t#68C` | `temperature` | degC; IPTS-68 |  |
| `t#68` | `temperature` | degC; IPTS-68 |  |
| `t#90C` | `temperature` | degC; ITS-90 |  |
| `t#90` | `temperature` | degC; ITS-90 |  |
| `upoly#` | `upoly` | \- |  |
| `user#` | `user` | \- |  |
| `v##` | `voltage` | V |  |
| `wetBAttn` | `beamAttenuation` | 1/m; WET Labs AC3 |  |
| `wetBTrans` | `beamTransmission` | percent; WET Labs AC3 |  |
| `wetCDOMdiff` | `fluorescenceDifference` | mg/m^3; WET Labs CDOM |  |
| `wetCDOM#` | `fluorescence` | mg/m^3; WET Labs CDOM |  |
| `wetChAbs` | `fluorescence` | 1/m; WET Labs AC3 absorption |  |
| `wetStardiff` | `fluorescenceDifference` | mg/m^3; WET Labs WETstar |  |
| `wetStar#` | `fluorescence` | mg/m^3; WET Labs WETstar |  |
| `xmiss` | `beamTransmission` | percent; Chelsea/Seatech |  |
| `xmiss#` | `beamTransmission` | percent; Chelsea/Seatech |  |

Notes:

1.  `pr` is in a Dalhousie-generated data file but seems not to be in
    reference 1.

2.  This is a strange unit, and so if `sw*` functions are called on an
    object containing this, a conversion will be made before performing
    the computation. Be on the lookout for errors, since this is a rare
    situation.

3.  Assume ITS-90 temperature scale, since sample `.cnv` file headers do
    not specify it.

4.  Some files have PSU for this. Should we handle that? And are there
    other S scales to consider?

5.  The 'theta' symbol (here shown accented e) may appear in different
    ways with different encoding configurations, set up within R or in
    the operating system.

## References

1.  A SBE data processing manual was once at
    `http://www.seabird.com/document/sbe-data-processing-manual`, but as
    of summer 2018, this no longer seems to be provided by SeaBird. A
    web search will turn up copies of the manual that have been put
    online by various research groups and data-archiving agencies. As of
    2018-07-05, the latest version was named
    `SBEDataProcessing_7.26.4.pdf` and had release date 12/08/2017, and
    this was the reference version used in coding `oce`.

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
[`ctd-class`](https://dankelley.github.io/oce/reference/ctd-class.md),
[`ctd.cnv.gz`](https://dankelley.github.io/oce/reference/ctd.cnv.gz.md),
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md),
[`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`ctdRaw`](https://dankelley.github.io/oce/reference/ctdRaw.md),
[`ctdRepair()`](https://dankelley.github.io/oce/reference/ctdRepair.md),
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md),
[`ctd_aml_type1.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type1.csv.gz.md),
[`ctd_aml_type3.csv.gz`](https://dankelley.github.io/oce/reference/ctd_aml_type3.csv.gz.md),
[`d200321-001.ctd.gz`](https://dankelley.github.io/oce/reference/d200321-001.ctd.gz.md),
[`d201211_0011.cnv.gz`](https://dankelley.github.io/oce/reference/d201211_0011.cnv.gz.md),
[`handleFlags,ctd-method`](https://dankelley.github.io/oce/reference/handleFlags-ctd-method.md),
[`initialize,ctd-method`](https://dankelley.github.io/oce/reference/initialize-ctd-method.md),
[`initializeFlagScheme,ctd-method`](https://dankelley.github.io/oce/reference/initializeFlagScheme-ctd-method.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`plot,ctd-method`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md),
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md),
[`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md),
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.odv()`](https://dankelley.github.io/oce/reference/read.ctd.odv.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

Other functions that interpret variable names and units from headers:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`unitFromString()`](https://dankelley.github.io/oce/reference/unitFromString.md),
[`unitFromStringRsk()`](https://dankelley.github.io/oce/reference/unitFromStringRsk.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)

## Author

Dan Kelley
