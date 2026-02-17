# Read an odf File

ODF (Ocean Data Format) is a format developed at the Bedford Institute
of Oceanography and also used at other Canadian Department of Fisheries
and Oceans (DFO) facilities (see references 1 and 2). It can hold
various types of time-series data, which includes a variety of
instrument types. Thus, `read.odf()` is used by `read.ctd.odf` for CTD
data, etc.

## Usage

``` r
read.odf(
  file,
  columns = NULL,
  header = "list",
  exclude = NULL,
  encoding = "latin1",
  debug = getOption("oceDebug")
)
```

## Arguments

- file:

  the file containing the data.

- columns:

  An optional [list](https://rdrr.io/r/base/list.html) that can be used
  to convert unrecognized data names to resultant variable names. For
  example,
  `columns=list(salinity=list(name="salt", unit=list(unit=expression(), scale="PSS-78"))`
  states that a short-name of `"salt"` represents salinity, and that the
  unit is as indicated. This is passed to
  [`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md)
  or
  [`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
  as appropriate, and takes precedence over the lookup table in that
  function.

- header:

  An indication of whether, or how, to store the entire ODF file header
  in the `metadata` slot of the returned object. There are three choices
  for the `header` argument. (1) If it is `NULL`, then the ODF header is
  not stored in the `metadata` slot (although some of its contents
  are). (2) If it is `"character"`, the header is stored within the
  `metadata` as a vector named `header`, comprising a character string
  for each line of the header within the ODF file. (3) If it is
  `"list"`, then the `metadata` slot of the returned object will contain
  a `list` named `header` that has lists as its entries. (The sub-lists
  are in the form of key-value pairs.) The naming of list entries is
  patterned on that in the ODF header, except that
  [`unduplicateNames()`](https://dankelley.github.io/oce/reference/unduplicateNames.md)
  is used to transform repeated names by adding numerical suffices.
  Note: on June 6, 2019, the default value of `header` was changed from
  `NULL` to `"list"`; in addition, the resultant list was made to
  contain every single item in the ODF header, with
  [`unduplicateNames()`](https://dankelley.github.io/oce/reference/unduplicateNames.md)
  being used to append integers to distinguish between repeated names in
  the ODF format.

- exclude:

  either a character value holding a regular expression that is used
  with [`grep()`](https://rdrr.io/r/base/grep.html) to remove lines from
  the header before processing, or `NULL` (the default), meaning not to
  exclude any such lines. The purpose of this argument is to solve
  problems with some files, which can have thousands of lines that
  indicate details that are may be of little value in processing. For
  example, some files have thousands of lines that would be excluded by
  using `exclude="PROCESS='Nulled the .* value"` in the function call.

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

## Value

An [oce](https://dankelley.github.io/oce/reference/oce-class.md) object.

## Details

Note that some elements of the metadata are particular to ODF objects,
e.g. `depthMin`, `depthMax` and `sounding`, which are inferred from ODF
items named `MIN_DEPTH`, `MAX_DEPTH` and `SOUNDING`, respectively. In
addition, the more common metadata item `waterDepth`, which is used in
`ctd` objects to refer to the total water depth, is set to `sounding` if
that is finite, or to `maxDepth` otherwise.

The function
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md)
is used to translate data names from the ODF file to standard `oce`
names.

## Metadata conventions

Some metadata items may be specific to certain instruments, and certain
research groups. It can be important for analysts to be aware of the
conventions used in datasets that are under study. For example, as of
June 2018, `adp` objects created at the Bedford Institute of
Oceanography may have a metadata item named `depthOffBottom` (called
`DEPTH_OFF_BOTTOM` in ODF files), which is not typically present in
`ctd` files. This item illustrates the renaming convention, from the
CAMEL_CASE used in ODF files to the snakeCase used in oce. Bearing this
conversion in mind, users should not find it difficult to understand the
meaning of items that `read.odf()` stores within the `metadata` slot.
Users should bear in mind that the whole ODF header is saved as a list
by calling the function with `header="list"`, after which e.g.
[str](https://rdrr.io/r/utils/str.html)`(rval[["header"]])` or
[View](https://rdrr.io/r/utils/View.html)`(rval[["header"]])` can be
used to isolate any information of interest (but bear in mind that
suffices are used to disambiguate sibling items of identical name in the
ODF header).

## Handling of temperature scales

`read.odf()` stores temperature data directly as read from the file,
which might mean the IPTS-68 scale. These values should not be used to
calculate other seawater quantities, because formulae are generally
based in ITS90 temperatures. To avoid problems, the accessor function
converts to the modern scale, e.g. `x[["temperature"]]` yields
temperature in the ITS90 scale, whether temperatures in the original
file were reported on that scale or the older IPTS-68 scale.

## Caution

Lacking detailed documentation of the ODF file format, the `read.odf()`
and
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md)
functions were crafted based on inspection of data files, and so some
guesses had to be made.

The `PARAMETER_HEADER` chunks describing quality-control flags are a
case in point. These contain `NAME` components that refer to other
`PARAMETER_HEADER` chunks that hold measured data. However, those
references are not always matched well with the data names, and even if
they do match, the cross-reference syntax used by the Bedford Institute
of Oceanography differs from that used by l’Institut Maurice-Lamontagne.
To simplify coding, it was assumed that each quality-control sequence
applies to the data sequence immediately preceding it. (This assumption
is made in other analysis systems.)

It is also prudent to pay attention to the units decoding, which
`read.odf()` handles by calling
[`unitFromString()`](https://dankelley.github.io/oce/reference/unitFromString.md).
Be on the lookout for incorrect temperature scales, which are sometimes
reported with nonstandard strings in ODF files. Also, note that you may
see warnings about conductivity ratios, which some ODF files incorrectly
suggest have dimensions.

## References

For sources that describe the ODF format, see the documentation for the
[odf](https://dankelley.github.io/oce/reference/odf-class.md) class.

## See also

[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md) will
be an alternative to this, once (or perhaps if) a `ODF` package is
released by the Canadian Department of Fisheries and Oceans.

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)

## Author

Dan Kelley, with help from Chantelle Layton

## Examples

``` r
library(oce)
#
# 1. Read a CTD cast made on the Scotian Shelf. Note that the file's metadata
# states that conductivity is in S/m, but it is really conductivity ratio,
# so we must alter the unit before converting to a CTD object. Note that
# read.odf() on this data file produces a warning suggesting that the user
# repair the unit, using the method outlined here.
odf <- read.odf(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package = "oce"))
#> Warning: "conductivity" (code name "CRAT_01") is a conductivity ratio, which has no units, but the file lists "s/m" as a unit. Consult ?read.odf to see how to rectify this error.
ctd <- as.ctd(odf) # so we can e.g. extract potential temperature
ctd[["conductivityUnit"]] <- list(unit = expression(), scale = "")
#
# 2. Make a CTD, and plot (with span to show NS)
plot(ctd, span = 500)

#
# 3. Highlight bad data on TS diagram. (Note that the eos
# is specified, because we will extract practical-salinity and
# UNESCO-defined potential temperatures for the added points.)
plotTS(ctd, type = "o", eos = "unesco") # use a line to show loops
bad <- ctd[["QCFlag"]] != 0
points(ctd[["salinity"]][bad], ctd[["theta"]][bad], col = "red", pch = 20)

```
