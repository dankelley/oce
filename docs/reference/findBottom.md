# Find the Ocean Bottom in an Echosounder Object

Finds the depth in a Biosonics echosounder file, by finding the
strongest reflector and smoothing its trace.

## Usage

``` r
findBottom(x, ignore = 5, clean = despike)
```

## Arguments

- x:

  an
  [echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
  object.

- ignore:

  number of metres of data to ignore, near the surface.

- clean:

  a function to clean the inferred depth of spikes.

## Value

A list with elements: the `time` of a ping, the `depth` of the inferred
depth in metres, and the `index` of the inferred bottom location,
referenced to the object's `depth` vector.

## See also

See the
[echosounder](https://dankelley.github.io/oce/reference/echosounder-class.md)
documentation to learn about the contents of such objects, and about
other functions that deal with them.

Other things related to echosounder data:
[`[[,echosounder-method`](https://dankelley.github.io/oce/reference/sub-sub-echosounder-method.md),
`[[<-,echosounder-method`,
[`as.echosounder()`](https://dankelley.github.io/oce/reference/as.echosounder.md),
[`echosounder`](https://dankelley.github.io/oce/reference/echosounder.md),
[`echosounder-class`](https://dankelley.github.io/oce/reference/echosounder-class.md),
[`plot,echosounder-method`](https://dankelley.github.io/oce/reference/plot-echosounder-method.md),
[`read.echosounder()`](https://dankelley.github.io/oce/reference/read.echosounder.md),
[`subset,echosounder-method`](https://dankelley.github.io/oce/reference/subset-echosounder-method.md),
[`summary,echosounder-method`](https://dankelley.github.io/oce/reference/summary-echosounder-method.md)

## Author

Dan Kelley
