# Read a ctd File in AML Format

`read.ctd.aml()` reads files that hold data acquired with an AML
Oceanographic Base.X2 CTD instrument. The software associated with this
device can output data in multiple formats, of which `read.ctd.aml()`
can read only three, based on files provided to the author by users. If
the `format` parameter is not supplied, the function attempts to infer
it from the first line in the file; see “Details”.

## Usage

``` r
read.ctd.aml(
  file,
  format,
  encoding = "UTF-8-BOM",
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- format:

  an integer indicating the format type. If not supplied, the first line
  is examined to make a guess as to the format (see “Details”).

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
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

- processingLog:

  ignored.

- ...:

  ignored.

## Value

`read.ctd.aml()` returns a
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md) object.

## Details

If `format` is not supplied, the first line of the file is examined. If
that line contains `[cast header]` (case insensitive), then format 1 is
inferred. If it contains a comma (i.e. if no header is present) then
format 2 is inferred. (The AML documentation cautions against saving in
this format.) And if it contains `[header]` (case insensitive) then
format 3 is inferred.

Support for types 1 and 2 were added in about the year 2017, whereas
type 3 was added in 2024. Documentation was once available for formats 1
and 2, but it was not an exact match to sample files provided to the
author of `read.ctd.aml()`. No documentation seemed to be available for
format 3, so the code was written after manual inspection of a sample
file. Given these things, users are advised to be on the lookout for
problems.

## References

1.  AML Oceanographic. "SeaCast 4 User Manual (Version 2.06)." AML
    Oceanographic, May 2016. This was once available at the
    \<www.subsseateechnologies.com\> website, but neither it nor a new
    version could be located by the author's search of the website in
    September 2024.

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

Other functions that read ctd data:
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
[`read.ctd.itp()`](https://dankelley.github.io/oce/reference/read.ctd.itp.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.ctd.saiv()`](https://dankelley.github.io/oce/reference/read.ctd.saiv.md),
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md),
[`read.ctd.ssda()`](https://dankelley.github.io/oce/reference/read.ctd.ssda.md),
[`read.ctd.woce()`](https://dankelley.github.io/oce/reference/read.ctd.woce.md),
[`read.ctd.woce.other()`](https://dankelley.github.io/oce/reference/read.ctd.woce.other.md)

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Show S,T and p for first 5 lines of a format=1 file
f1 <- system.file("extdata", "ctd_aml_type1.csv.gz", package = "oce")
d1 <- read.ctd.aml(f1)
data.frame(S = d1[["salinity"]], T = d1[["temperature"]], p = d1[["pressure"]])
#>           S     T    p
#> 1  5.577871 5.671 0.23
#> 2  9.978261 5.509 0.22
#> 3 14.628767 5.400 0.22
#> 4 19.406751 5.325 0.24

# Show S,T and p for first 5 lines of a format=3 file
f3 <- system.file("extdata", "ctd_aml_type3.csv.gz", package = "oce")
d3 <- read.ctd.aml(f3)
data.frame(S = d3[["salinity"]], T = d3[["temperature"]], p = d3[["pressure"]])
#>        S      T    p
#> 1 37.797 25.871 0.00
#> 2 37.907 25.890 0.19
#> 3 37.914 25.861 0.44
#> 4 37.920 25.862 0.68
#> 5 37.932 25.838 0.90
```
