# Coerce Data Into a ctd Object

Assemble data into a
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.
There are two ways this can work. First, `salinity` can be a vector of
numeric values, in which case the other parameters will be interpreted
as described below. Second, `salinity` can be an
[oce](https://dankelley.github.io/oce/reference/oce-class.md) object, in
which case the action depends on the object class, as described in the
‘Details’.

## Usage

``` r
as.ctd(
  salinity,
  temperature = NULL,
  pressure = NULL,
  conductivity = NULL,
  scan = NULL,
  time = NULL,
  units = NULL,
  flags = NULL,
  missingValue = NULL,
  type = "",
  serialNumber = NULL,
  ship = NULL,
  cruise = NULL,
  station = NULL,
  startTime = NULL,
  longitude = NULL,
  latitude = NULL,
  deploymentType = "unknown",
  pressureAtmospheric = 0,
  sampleInterval = NULL,
  profile = NULL,
  debug = getOption("oceDebug")
)
```

## Arguments

- salinity:

  may be (1) a numeric vector holding Practical Salinity, (2) a list or
  data frame holding `salinity` and other hydrographic variables or (3)
  an `oce-class` object that holds hydrographic information. If
  `salinity` is not provided, then `conductivity` must be provided, so
  that [`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md)
  can be used to compute salinity.

- temperature:

  a numeric vector containing *in-situ* temperature in \\^\circ\\C on
  the ITS-90 scale; see “Temperature units” in the documentation for
  [`swRho()`](https://dankelley.github.io/oce/reference/swRho.md).

- pressure:

  a numeric vector containing sea pressure values, in decibars.
  Typically, this vector has the same length as `salinity` and
  `temperature`, but it also possible to supply just one value, which
  will be repeated to get the right length. Note that `as.ctd()` stores
  the sum of `pressure` and `pressureAtmospheric` in the returned
  object, although the default value for `pressureAtmospheric` is zero,
  so in the default case, `pressure` is stored directly.

- conductivity:

  an optional numeric vector containing electrical conductivity ratio
  through the water column. To convert from raw conductivity in
  milliSeimens per centimeter divide by 42.914 to get conductivity ratio
  (see Culkin and Smith, 1980).

- scan:

  optional numeric vector holding scan number. If not provided, this is
  set to [seq_along](https://rdrr.io/r/base/seq.html)`(salinity)`.

- time:

  optional vector of times of observation.

- units:

  an optional list containing units. If not supplied, defaults are set
  for `pressure`, `temperature`, `salinity`, and `conductivity`. Since
  these are simply guesses, users are advised strongly to supply
  `units`. See “Examples”.

- flags:

  if supplied, this is a [list](https://rdrr.io/r/base/list.html)
  containing data-quality flags. The elements of this list must have
  names that match the data provided to the object.

- missingValue:

  optional missing value, indicating data that should be taken as `NA`.
  Set to `NULL` to turn off this feature.

- type:

  optional type of CTD, e.g. "SBE"

- serialNumber:

  optional serial number of instrument

- ship:

  optional string containing the ship from which the observations were
  made.

- cruise:

  optional string containing a cruise identifier.

- station:

  optional string containing a station identifier.

- startTime:

  optional indication of the start time for the profile, which is used
  in some several plotting functions. This is best given as a
  [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html) time, but it may
  also be a character string that can be converted to a time with
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), using `UTC`
  as the timezone.

- longitude:

  optional numerical value containing longitude in decimal degrees,
  positive in the eastern hemisphere. If this is a single number, then
  it is stored in the `metadata` slot of the returned value; if it is a
  vector of numbers, then they are stored in the `data` slot. If
  `longitude' is not provided (i.e. if it is NULL, the default), then `as.ctd()'
  tries to find it from the first parameter, if it is a list, or an
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) object.

- latitude:

  similar to `longitude`. Positive in the northern hemisphere.

- deploymentType:

  character string indicating the type of deployment. Use `"unknown"` if
  this is not known, `"profile"` for a profile (in which the data were
  acquired during a downcast, while the device was lowered into the
  water column, perhaps also including an upcast; `"moored"` if the
  device is installed on a fixed mooring, `"thermosalinograph"` (or
  `"tsg"`) if the device is mounted on a moving vessel, to record
  near-surface properties, or `"towyo"` if the device is repeatedly
  lowered and raised.

- pressureAtmospheric:

  A numerical value (a constant or a vector), that is subtracted from
  pressure before storing it in the return value. (This altered pressure
  is also used in calculating `salinity`, if that is to be computed from
  `conductivity`, etc., using
  [`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md); see
  `salinity` above.)

- sampleInterval:

  optional numerical value indicating the time between samples in the
  profile.

- profile:

  optional positive integer specifying the number of the profile to
  extract from an object that has data in matrices, such as for some
  `argo` objects. Currently the `profile` argument is only utilized for
  [argo](https://dankelley.github.io/oce/reference/argo-class.md)
  objects.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A [ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.

## Details

If the first parameter, `salinity`, is an
[oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
then the action depends on the class of that object.

1.  If `salinity` is
    [ctd](https://dankelley.github.io/oce/reference/ctd-class.md)
    object, then \`as.ctd()1 returns a copy of it.

2.  If `salinity` is an
    [argo](https://dankelley.github.io/oce/reference/argo-class.md)
    object, then `as.ctd()` calls
    [`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md)
    with that object as its first parameter, along with the value of
    `profile` and the value of `debug` minus 1. All other parameters
    provided to `as.ctd()` are ignored. Note that Argo notation is
    retained in the return value, so that e.g. there is no metadata item
    named `station` (instead, `id` and `cycleNumber` are defined), and
    no item named `startTime` (instead, `time` is defined. These name
    changes are understood by the
    [`summary()`](https://rdrr.io/r/base/summary.html) and
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html) functions.
    **Breaking Change:** Until version 1.8-4, `as.ctd()` also processed
    the parameters that are ignored now. This behaviour was changed
    because many of those parameters (e.g. `cruise` and `ship`) make no
    sense for Argo data. Users should now use
    [`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md)
    to insert additional items as desired.

3.  If `salinity` is an
    [rsk](https://dankelley.github.io/oce/reference/rsk-class.md)
    object, then `as.ctd()` calls
    [`rsk2ctd()`](https://dankelley.github.io/oce/reference/rsk2ctd.md)
    with that object as its first argument, along with
    `pressureAtmospheric`, `longitude`, `latitude` and `debug` minus 1,
    ignoring all the other parameters. Note that pressure in the
    returned object may need to be adjusted, because `rsk` objects may
    contain either absolute pressure or sea pressure. This adjustment is
    handled automatically by `as.ctd`, by examination of the metadata
    item named `pressureType` (described in the documentation for
    [`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md)).
    Once the sea pressure is determined, adjustments may be made with
    the `pressureAtmospheric` argument, although in that case it is
    better considered a pressure adjustment than the atmospheric
    pressure.

## References

Culkin, F., and Norman D. Smith, 1980. Determination of the
concentration of potassium chloride solution having the same electrical
conductivity, at 15 C and infinite frequency, as standard seawater of
salinity 35.0000 ppt (Chlorinity 19.37394 ppt). *IEEE Journal of Oceanic
Engineering*, volume **5**, pages 22-23.

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
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

## Author

Dan Kelley, with help from Clark Richards

## Examples

``` r
library(oce)
# 1. fake data, with default units
pressure <- 1:50
temperature <- 10 - tanh((pressure - 20) / 5) + 0.02 * rnorm(50)
salinity <- 34 + 0.5 * tanh((pressure - 20) / 5) + 0.01 * rnorm(50)
ctd <- as.ctd(salinity, temperature, pressure)
# Add a new column
fluo <- 5 * exp(-pressure / 20)
ctd <- oceSetData(ctd,
    name = "fluorescence", value = fluo,
    unit = list(unit = expression(mg / m^3), scale = "")
)
summary(ctd)
#> CTD Summary
#> -----------
#> 
#> * Data Overview
#> 
#>                              Min.    Mean   Max.   Dim. NAs OriginalName
#>     scan                     1       25.5   50     50   0   "-"         
#>     salinity [PSS-78]        33.489  34.11  34.521 50   0   "-"         
#>     temperature [°C, ITS-90] 8.9673  9.778  11.029 50   0   "-"         
#>     pressure [dbar]          1       25.5   50     50   0   "-"         
#>     fluorescence [mg/m³]     0.41042 1.7903 4.7561 50   0   "-"         
#> 
#> * Processing Log
#> 
#>     - 2026-04-11 11:44:56 UTC: `create 'ctd' object`
#>     - 2026-04-11 11:44:56 UTC: `as.ctd(salinity = salinity, temperature = temperature, pressure = pressure)`
#>     - 2026-04-11 11:44:56 UTC: `oceSetData(object = ctd, name = "fluorescence", value = fluo,     unit = list(unit = expression(mg/m^3), scale = ""))`

# 2. fake data, with supplied units (which are the defaults, actually)
ctd <- as.ctd(salinity, temperature, pressure,
    units = list(
        salinity = list(unit = expression(), scale = "PSS-78"),
        temperature = list(unit = expression(degree * C), scale = "ITS-90"),
        pressure = list(unit = expression(dbar), scale = "")
    )
)
```
