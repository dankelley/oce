# Create ODF Object From Output of read_ODF in ODF package

As of August 11, 2015, `ODF::read_ODF` returns a list with 9 elements,
one named `DATA`, which is a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) containing the
columnar data, the others being headers of various sorts. The present
function constructs an oce object from such data, facilitating
processing and plotting with the general oce functions. This involves
storing the 8 headers verbatim in the `odfHeaders` in the `metadata`
slot, and also copying some of the header information into more standard
names (e.g. `metadata@longitude` is a copy of
`metadata@odfHeader$EVENT_HEADER$INITIAL_LATITUDE`). As for the `DATA`,
they are stored in the `data` slot, after renaming from ODF to oce
convention using
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md).

## Usage

``` r
ODF2oce(ODF, coerce = TRUE, debug = getOption("oceDebug"))
```

## Arguments

- ODF:

  A list as returned by `read_ODF` in the `ODF` package

- coerce:

  A logical value indicating whether to coerce the return value to an
  appropriate object type, if possible.

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

## Value

An oce object, possibly coerced to a subtype.

## Caution

This function may change as the `ODF` package changes. Since `ODF` has
not been released yet, this should not affect any users except those
involved in the development of `oce` and `ODF`.

## See also

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODFListFromHeader()`](https://dankelley.github.io/oce/reference/ODFListFromHeader.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
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
