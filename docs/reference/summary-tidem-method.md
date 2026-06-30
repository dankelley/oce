# Summarize a tidem Object

By default, all fitted constituents are plotted, but it is quite useful
to set e.g. p=0.05 To see just those constituents that are significant
at the 5 percent level. Note that the p values are estimated as the
average of the p values for the sine and cosine components at a given
frequency.

## Usage

``` r
# S4 method for class 'tidem'
summary(object, p = 1, constituent, ...)
```

## Arguments

- object:

  an object of class
  [tidem](https://dankelley.github.io/oce/reference/tidem.md), as
  created by
  [`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md)
  or [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md).

- p:

  optional value of the maximum p value for the display of an individual
  coefficient. If not given, all coefficients are shown.

- constituent:

  optional character vector holding the names of constituents on which
  to focus.

- ...:

  further arguments passed to or from other methods.

## Value

`NULL`

## Sample of Usage


    library(oce)
    data(sealevel)
    tide <- tidem(sealevel)
    summary(tide)

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`predict.tidem()`](https://dankelley.github.io/oce/reference/predict.tidem.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley
