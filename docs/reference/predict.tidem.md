# Predict a Tidal Signal

This creates a time-series of predicted tides, based on a tidal model
object that was created by
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md) or
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md).

## Usage

``` r
# S3 method for class 'tidem'
predict(object, newdata, ...)
```

## Arguments

- object:

  a [tidem](https://dankelley.github.io/oce/reference/tidem-class.md)
  object.

- newdata:

  vector of POSIXt times at which to make the prediction. For models
  created with
  [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md), the
  `newdata` argument is optional, and if it is not provided, then the
  predictions are at the observation times given to
  [`tidem()`](https://dankelley.github.io/oce/reference/tidem.md).
  However, `newdata` is required if
  [`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md)
  had been used to create `object`.

- ...:

  optional arguments passed on to children.

## Value

A vector of predictions.

## Details

All the tidal constituents that are stored in `object` are used, not
just those that are statistically significant or that have amplitude
exceeding any particular value. In this respect, `predict.tidem()`
follows a pattern established by e.g.
[`predict.lm()`](https://rdrr.io/r/stats/predict.lm.html). Note that the
constituents in `object` are straightforward if it was constructed with
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
but considerably more complicated for
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md), and so
the documentation for the latter ought to be studied closely, especially
with regard to the Rayleigh criterion.

## Sample of Usage

    # prediction at specified times
    data(sealevel)
    m <- tidem(sealevel)
    # Check fit over 2 days (interpolating to finer timescale)
    look <- 1:48
    time <- sealevel[["time"]]
    elevation <- sealevel[["elevation"]]
    oce.plot.ts(time[look], elevation[look])
    # 360s = 10 minute timescale
    t <- seq(from=time[1], to=time[max(look)], by=360)
    lines(t, predict(m, newdata=t), col="red")
    legend("topright", col=c("black","red"),
    legend=c("data","model"),lwd=1)

## See also

Other things related to tides:
[`[[,tidem-method`](https://dankelley.github.io/oce/reference/sub-sub-tidem-method.md),
`[[<-,tidem-method`,
[`as.tidem()`](https://dankelley.github.io/oce/reference/as.tidem.md),
[`plot,tidem-method`](https://dankelley.github.io/oce/reference/plot-tidem-method.md),
[`summary,tidem-method`](https://dankelley.github.io/oce/reference/summary-tidem-method.md),
[`tidalCurrent`](https://dankelley.github.io/oce/reference/tidalCurrent.md),
[`tidedata`](https://dankelley.github.io/oce/reference/tidedata.md),
[`tidem`](https://dankelley.github.io/oce/reference/tidem.md),
[`tidem-class`](https://dankelley.github.io/oce/reference/tidem-class.md),
[`tidemAstron()`](https://dankelley.github.io/oce/reference/tidemAstron.md),
[`tidemVuf()`](https://dankelley.github.io/oce/reference/tidemVuf.md),
[`webtide()`](https://dankelley.github.io/oce/reference/webtide.md)

## Author

Dan Kelley

## Examples

``` r
# Show non-tidal sealevel signal in Halifax Harbour during
# the year 2002. The spike resulted from Hurricane Juan.
library(oce)
data(sealevel)
time <- sealevel[["time"]]
elevation <- sealevel[["elevation"]]
prediction <- tidem(sealevel) |> predict()
#> Warning: tidal record too short to fit constituents: SA, PI1, S1, PSI1, GAM2, H1, H2, T2, R2
oce.plot.ts(time, elevation - prediction)

```
