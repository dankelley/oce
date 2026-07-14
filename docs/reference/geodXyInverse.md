# Inverse Geodesic Calculation

The calculation is done by finding a minimum value of a cost function
that is the vector difference between (`x`,`y`) and the corresponding
values returned by
[`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md). See
“Caution”.

## Usage

``` r
geodXyInverse(x, y, longitudeRef, latitudeRef, debug = getOption("oceDebug"))
```

## Arguments

- x:

  value of x in metres, as given by
  [`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)

- y:

  value of y in metres, as given by
  [`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)

- longitudeRef:

  reference longitude, as supplied to
  [`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)

- latitudeRef:

  reference latitude, as supplied to
  [`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

a data frame containing `longitude` and `latitude`

## Details

The minimum is calculated in C for speed, using the `nmmin` function
that is the underpinning for the Nelder-Meade version of the R function
[`optim()`](https://rdrr.io/r/stats/optim.html). If you find odd
results, try setting `debug=1` and rerunning, to see whether this
optimizer is having difficulty finding a minimum of the mismatch
function.

## Caution

This scheme is without known precedent in the literature, and users
should read the documentation carefully before deciding to use it.

## See also

Other functions relating to geodesy:
[`geodDist()`](https://dankelley.github.io/oce/reference/geodDist.md),
[`geodGc()`](https://dankelley.github.io/oce/reference/geodGc.md),
[`geodXy()`](https://dankelley.github.io/oce/reference/geodXy.md)
