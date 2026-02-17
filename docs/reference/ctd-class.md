# Class to Store CTD (or general hydrographic) Data

This class stores hydrographic data such as measured with a CTD
(conductivity, temperature, depth) instrument, or with other systems
that produce similar data. Data repositories may store conductivity,
temperature and depth, as in the instrument name, but it is also common
to store salinity, temperature and pressure instead (or in addition).
For this reason, `ctd` objects are required to hold `salinity`,
`temperature` and `pressure` in their `data` slot, with other data being
optional. Formulae are available for converting between variants of
these data triplets, e.g.
[`swSCTp()`](https://dankelley.github.io/oce/reference/swSCTp.md) can
calculate `salinity` given `conductivity`, `temperature` and `pressure`,
and these are used by the main functions that create `ctd` objects. For
example, if
[`read.ctd.sbe()`](https://dankelley.github.io/oce/reference/read.ctd.sbe.md)
is used to read a Seabird file that contains only conductivity,
temperature and pressure, then that function will automatically append a
data item to hold salinity. Since
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md) does
the same with salinity, the result this is that all `ctd` objects hold
`salinity`, `temperature` and `pressure`, which are henceforth called
the three basic quantities.

## Details

Different units and scales are permitted for the three basic quantities,
and most `oce` functions check those units and scales before doing
calculations (e.g. of seawater density), because those calculations
demand certain units and scales. The way this is handled is that the
accessor function
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md)\]
returns values in standardized form. For example, a `ctd` object might
hold temperature defined on the IPTS-68 scale, but e.g.
`ctd[["temperature"]]` returns a value on the ITS-90 scale. (The
conversion is done with
[`T90fromT68()`](https://dankelley.github.io/oce/reference/T90fromT68.md).)
Similarly, pressure may be stored in either dbars or PSI, but e.g.
`ctd[["pressure"]]` returns a value in dbars, after dividing by 0.689476
if the value is stored in PSI. Luckily, there is (as of early 2016) only
one salinity scale in common use in data files, namely PSS-78.

## Slots

- `data`:

  As with all `oce` objects, the `data` slot for `ctd` objects is a
  [list](https://rdrr.io/r/base/list.html) containing the main data for
  the object. The key items stored in this slot are: `salinity`,
  `temperature`, and `pressure`, although in many instances there are
  quite a few additional items.

- `metadata`:

  As with all `oce` objects, the `metadata` slot for `ctd` objects is a
  [list](https://rdrr.io/r/base/list.html) containing information about
  the `data` or about the object itself. An example of the former might
  be the location at which a `ctd` measurement was made, stored in
  `longitude` and `latitude`, and of the latter might be `filename`, the
  name of the data source.

- `processingLog`:

  As with all `oce` objects, the `processingLog` slot for `ctd` objects
  is a [list](https://rdrr.io/r/base/list.html) with entries describing
  the creation and evolution of the object. The contents are updated by
  various `oce` functions to keep a record of processing steps. Object
  summaries and
  [`processingLogShow()`](https://dankelley.github.io/oce/reference/processingLogShow.md)
  both display the log.

## Modifying slot contents

Although the `[[<-` operator may permit modification of the contents of
ctd objects (see `[[<-,ctd-method`), it is better to use
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md),
because those functions save an entry in the `processingLog` that
describes the change.

## Retrieving slot contents

The full contents of the `data` and `metadata` slots of a ctd object may
be retrieved in the standard R way using
[`slot()`](https://rdrr.io/r/methods/slot.html). For example
`slot(o,"data")` returns the `data` slot of an object named `o`, and
similarly `slot(o,"metadata")` returns the `metadata` slot.

The slots may also be obtained with the
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md)
operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.

The
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md)
operator can also be used to retrieve items from within the `data` and
`metadata` slots. For example, `o[["temperature"]]` can be used to
retrieve temperature from an object containing that quantity. The rule
is that a named quantity is sought first within the object's `metadata`
slot, with the `data` slot being checked only if `metadata` does not
contain the item. This `[[` method can also be used to get certain
derived quantities, if the object contains sufficient information to
calculate them. For example, an object that holds (practical) salinity,
temperature and pressure, along with longitude and latitude, has
sufficient information to compute Absolute Salinity, and so `o[["SA"]]`
will yield the calculated Absolute Salinity.

It is also possible to find items more directly, using
[`oceGetData()`](https://dankelley.github.io/oce/reference/oceGetData.md)
and
[`oceGetMetadata()`](https://dankelley.github.io/oce/reference/oceGetMetadata.md),
but neither of these functions can retrieve derived items.

## Reading/creating `ctd` objects

A file containing CTD profile data may be read with
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md),
and a CTD object can also be created with
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md). See
[`read.ctd()`](https://dankelley.github.io/oce/reference/read.ctd.md)
for references on data formats used in CTD files. Data can also be
assembled into `ctd` objects with
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md).

Statistical summaries are provided by
[`summary,ctd-method()`](https://dankelley.github.io/oce/reference/summary-ctd-method.md),
while [`show()`](https://rdrr.io/r/methods/show.html) displays an
overview.

CTD objects may be plotted with
[`plot,ctd-method()`](https://dankelley.github.io/oce/reference/plot-ctd-method.md),
which does much of its work by calling
[`plotProfile()`](https://dankelley.github.io/oce/reference/plotProfile.md)
or [`plotTS()`](https://dankelley.github.io/oce/reference/plotTS.md),
both of which can also be called by the user, to get fine control over
the plots.

A CTD profile can be isolated from a larger record with
[`ctdTrim()`](https://dankelley.github.io/oce/reference/ctdTrim.md), a
task made easier when
[`plotScan()`](https://dankelley.github.io/oce/reference/plotScan.md) is
used to examine the results. Towyow data can be split up into sets of
profiles (ascending or descending) with
[`ctdFindProfiles()`](https://dankelley.github.io/oce/reference/ctdFindProfiles.md).
CTD data may be smoothed and/or cast onto specified pressure levels with
[`ctdDecimate()`](https://dankelley.github.io/oce/reference/ctdDecimate.md).

As with all oce objects, low-level manipulation may be done with
[`oceSetData()`](https://dankelley.github.io/oce/reference/oceSetData.md)
and
[`oceSetMetadata()`](https://dankelley.github.io/oce/reference/oceSetMetadata.md).
Additionally, many of the contents of CTD objects may be altered with
the `[[<-,ctd-method` scheme, and sufficiently skilled users may even
manipulate the contents directly.

## Data sources

Archived CTD (and other) data may be found on servers such as

1.  `https://cchdo.ucsd.edu/`

## See also

Other things related to ctd data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`[[,ctd-method`](https://dankelley.github.io/oce/reference/sub-sub-ctd-method.md),
`[[<-,ctd-method`,
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`ctd`](https://dankelley.github.io/oce/reference/ctd.md),
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

Other classes provided by oce:
[`adp-class`](https://dankelley.github.io/oce/reference/adp-class.md),
[`adv-class`](https://dankelley.github.io/oce/reference/adv-class.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`bremen-class`](https://dankelley.github.io/oce/reference/bremen-class.md),
[`cm-class`](https://dankelley.github.io/oce/reference/cm-class.md),
[`coastline-class`](https://dankelley.github.io/oce/reference/coastline-class.md),
[`lisst-class`](https://dankelley.github.io/oce/reference/lisst-class.md),
[`lobo-class`](https://dankelley.github.io/oce/reference/lobo-class.md),
[`met-class`](https://dankelley.github.io/oce/reference/met-class.md),
[`oce-class`](https://dankelley.github.io/oce/reference/oce-class.md),
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`section-class`](https://dankelley.github.io/oce/reference/section-class.md),
[`topo-class`](https://dankelley.github.io/oce/reference/topo-class.md),
[`windrose-class`](https://dankelley.github.io/oce/reference/windrose-class.md),
[`xbt-class`](https://dankelley.github.io/oce/reference/xbt-class.md)

## Author

Dan Kelley

## Examples

``` r
# 1. Create a ctd object with fake data.
a <- as.ctd(salinity = 35 + 1:3 / 10, temperature = 10 - 1:3 / 10, pressure = 1:3)
summary(a)
#> CTD Summary
#> -----------
#> 
#> * Data Overview
#> 
#>                              Min. Mean Max. Dim. NAs
#>     scan                     1    2    3    3    0  
#>     salinity [PSS-78]        35.1 35.2 35.3 3    0  
#>     temperature [°C, ITS-90] 9.7  9.8  9.9  3    0  
#>     pressure [dbar]          1    2    3    3    0  
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 20:56:56 UTC: `create 'ctd' object`
#>     - 2026-02-17 20:56:56 UTC: `as.ctd(salinity = 35 + 1:3/10, temperature = 10 - 1:3/10, pressure = 1:3)`

# 2. Fix a typo in a station latitude (fake! it's actually okay)
data(ctd)
ctd <- oceSetMetadata(
    ctd, "latitude", ctd[["latitude"]] - 0.001,
    "fix latitude typo in log book"
)
```
