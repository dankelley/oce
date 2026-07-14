# Create a List of odf Header Metadata

Create a List of odf Header Metadata

## Usage

``` r
ODFListFromHeader(header)
```

## Arguments

- header:

  Vector of character strings, holding the header

## Value

A list holding the metadata, with item names matching those in the ODF
header, except that duplicates are transformed through the use of
[`unduplicateNames()`](https://dankelley.github.io/oce/reference/unduplicateNames.md).

## See also

Other things related to odf data:
[`CTD_BCD2014666_008_1_DN.ODF.gz`](https://dankelley.github.io/oce/reference/CTD_BCD2014666_008_1_DN.ODF.gz.md),
[`ODF2oce()`](https://dankelley.github.io/oce/reference/ODF2oce.md),
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`[[,odf-method`](https://dankelley.github.io/oce/reference/sub-sub-odf-method.md),
`[[<-,odf-method`,
[`odf-class`](https://dankelley.github.io/oce/reference/odf-class.md),
[`plot,odf-method`](https://dankelley.github.io/oce/reference/plot-odf-method.md),
[`read.ctd.odf()`](https://dankelley.github.io/oce/reference/read.ctd.odf.md),
[`read.odf()`](https://dankelley.github.io/oce/reference/read.odf.md),
[`subset,odf-method`](https://dankelley.github.io/oce/reference/subset-odf-method.md),
[`summary,odf-method`](https://dankelley.github.io/oce/reference/summary-odf-method.md)
