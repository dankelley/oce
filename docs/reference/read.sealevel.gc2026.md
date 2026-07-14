# Read a sealevel File in Government of Canada format 2026

This is somewhat provisional code, to handle what seems to be a new
format as of the year 2026. In this format, files come in pairs, and so
this function takes just the station identifier and seeks files with
names related to that, according to a pattern observed on 2026-02-17.
Whether this pattern will hold is unclear.

## Usage

``` r
read.sealevel.gc2026(file, debug = 0)
```

## Arguments

- file:

  string vector of length 2, holding the names of the metadata file and
  the data file.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

A
[sealevel](https://dankelley.github.io/oce/reference/sealevel-class.md)
object.

## Details

The two input files are constructed from the value of the `stn`
parameter. For example, with
`stn="00491", the constructed filenames will be `"00491_metadata.csv"`and`"00491_data.csv"`. Note that the first of these is read, with contents inserted into the `metadata\`
of the returned object, but the items contained therein are not
otherwise examined by this function. Instead, the function determines
station location from a header contained within the "data" file.

## See also

Other things related to sealevel data:
[`[[,sealevel-method`](https://dankelley.github.io/oce/reference/sub-sub-sealevel-method.md),
`[[<-,sealevel-method`,
[`as.sealevel()`](https://dankelley.github.io/oce/reference/as.sealevel.md),
[`plot,sealevel-method`](https://dankelley.github.io/oce/reference/plot-sealevel-method.md),
[`read.sealevel()`](https://dankelley.github.io/oce/reference/read.sealevel.md),
[`sealevel`](https://dankelley.github.io/oce/reference/sealevel.md),
[`sealevel-class`](https://dankelley.github.io/oce/reference/sealevel-class.md),
[`sealevelTuktoyaktuk`](https://dankelley.github.io/oce/reference/sealevelTuktoyaktuk.md),
[`subset,sealevel-method`](https://dankelley.github.io/oce/reference/subset-sealevel-method.md),
[`summary,sealevel-method`](https://dankelley.github.io/oce/reference/summary-sealevel-method.md)

## Author

Dan Kelley and Chantelle Layton
