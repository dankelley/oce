# Read a ctd File in WOCE-Exchange Format

This reads WOCE exchange files that start with the string `"CTD"`. There
are two variants: one in which the first 4 characters are `"CTD,"` and
the other in which the first 3 characters are again `"CTD"` but no other
non-whitespace characters occur on the line.

## Usage

``` r
read.ctd.woce(
  file,
  columns = NULL,
  station = NULL,
  missingValue,
  deploymentType = "unknown",
  monitor = FALSE,
  encoding = "latin1",
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
```

## Arguments

- file:

  either a connection or a character value naming a file. For
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  and `read.ctd.woce()`, this may be a wildcard (e.g. `"*.cnv"` or
  `"*.csv"`) in which case the return value is a vector containing CTD
  objects created by reading the files from
  [`list.files()`](https://rdrr.io/r/base/list.files.html) with
  `pattern` set to the specified wildcard pattern.

- columns:

  an optional [list](https://rdrr.io/r/base/list.html) that can be used
  to convert unrecognized data names to resultant variable names. This
  is used only by
  [`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
  and
  [`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md).
  For example, if a data file named salinity as `"SAL"`, then using

      d <- read.ctd(f, columns=list(
          salinity=list(name="SAL",
                        unit=list(unit=expression(),
                        scale="PSS-78"))))

  would assign the `"SAL"` column to the `salinity` entry in the data
  slot of the CTD object returned by the `read.*` function.

- station:

  optional character string containing an identifying name or number for
  the station. This can be useful if the routine cannot determine the
  name automatically, or if another name is preferred.

- missingValue:

  optional missing-value flag; data matching this value will be set to
  `NA` upon reading. If this is provided, then it overrules any
  missing-value flag found in the data. For Seabird (`.cnv`) files,
  there is usually no need to set `missingValue`, because it can be
  inferred from the header (typically as -9.990e-29). Set
  `missingValue=NULL` to turn off missing-value detection, even in
  `.cnv` files that contain missing-value codes in their headers. If
  `missingValue` is not specified, then an attempt is made to infer such
  a value from the data, by testing whether salinity and/or temperature
  has a minimum that is under -8 in value; this should catch common
  values in files, without false positives. A warning will be issued in
  this case, and a note inserted in the processing log of the return
  value.

- deploymentType:

  character string indicating the type of deployment. Use `"unknown"` if
  this is not known, `"profile"` for a profile (in which the data were
  acquired during a downcast, while the device was lowered into the
  water column, perhaps also including an upcast; `"moored"` if the
  device is installed on a fixed mooring, `"thermosalinograph"` (or
  `"tsg"`) if the device is mounted on a moving vessel, to record
  near-surface properties, or `"towyo"` if the device is repeatedly
  lowered and raised.

- monitor:

  boolean, set to `TRUE` to provide an indication of progress. This is
  useful if `filename` is a wildcard.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed.

- processingLog:

  if provided, the action item to be stored in the log. This is
  typically only provided for internal calls; the default that it
  provides is better for normal calls by a user.

- ...:

  additional arguments, passed to called routines.

## Value

This function returns a
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.

## References

The WOCE-exchange format was once described at
`http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
although that link is no longer working as of December 2020.

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
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
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md),
[`setFlags,ctd-method`](https://dankelley.github.io/oce/reference/setFlags-ctd-method.md),
[`subset,ctd-method`](https://dankelley.github.io/oce/reference/subset-ctd-method.md),
[`summary,ctd-method`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md),
[`write.ctd()`](https://dankelley.github.io/oce/reference/write.ctd.md)

Other functions that read ctd data:
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.aml()`](https://dankelley.github.io/oce/reference/read.ctd.aml.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md)

## Author

Dan Kelley
