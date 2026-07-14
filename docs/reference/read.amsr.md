# Read an amsr File

Read an amsr file, generating an
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) object.
Two file types are handled: type 1 is from gzipped files that were
available until perhaps the year 2022, and type 2 is from NetCDF files
that were available afterwards. The type is stored in the `metadata`
slot as `type`, and this is detected in other functions relating to
[amsr](https://dankelley.github.io/oce/reference/amsr-class.md) data.
The best way to locate amsr files is to use
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
but if this fails, it may be necessary to search the web for a source.

## Usage

``` r
read.amsr(file, encoding = NA, debug = getOption("oceDebug"))
```

## Arguments

- file:

  String indicating the name of a compressed file. See “File sources”.

- encoding:

  ignored.

- debug:

  A debugging flag, integer.

## See also

[`plot,amsr-method()`](https://dankelley.github.io/oce/reference/plot-amsr-method.md)
for an example.

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`download.amsr()`](https://dankelley.github.io/oce/reference/download.amsr.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

## Author

Dan Kelley and Chantelle Layton
