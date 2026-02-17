# Coerce Data Into a rsk Object

Create a rsk object.

## Usage

``` r
as.rsk(
  time,
  columns,
  filename = "",
  instrumentType = "rbr",
  serialNumber = "",
  model = "",
  sampleInterval = NA,
  debug = getOption("oceDebug")
)
```

## Arguments

- time:

  a vector of times for the data.

- columns:

  a list or data frame containing the measurements at the indicated
  times; see “Details”.

- filename:

  optional name of file containing the data.

- instrumentType:

  type of instrument.

- serialNumber:

  serial number for instrument.

- model:

  instrument model type, e.g. `"RBRduo"`.

- sampleInterval:

  sampling interval. If given as `NA`, then this is estimated as the
  median difference in times.

- debug:

  a flag that can be set to `TRUE` to turn on debugging.

## Value

An [rsk](https://dankelley.github.io/oce/reference/rsk-class.md) object.

## Details

The contents of `columns` are be copied into the `data` slot of the
returned object directly, so it is critical that the names and units
correspond to those expected by other code dealing with
[rsk](https://dankelley.github.io/oce/reference/rsk-class.md) objects.
If there is a conductivity, it must be called `conductivity`, and it
must be in units of mS/cm. If there is a temperature, it must be called
`temperature`, and it must be an in-situ value recorded in ITS-90 units.
And if there is a pressure, it must be *absolute* pressure (sea pressure
plus atmospheric pressure) and it must be named `pressure`. No checks
are made within `as.rsk` on any of these rules, but if they are broken,
you may expect problems with any further processing.

## See also

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`rskToc()`](https://dankelley.github.io/oce/reference/rskToc.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

## Author

Dan Kelley
