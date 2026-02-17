# Window an oce Object by Time or Distance

Windows `x` on either time or distance, depending on the value of
`which`. In each case, values of `start` and `end` may be integers, to
indicate a portion of the time or distance range. If `which` is
`"time"`, then the `start` and `end` values may also be provided as
POSIX times, or character strings indicating times (in time zone given
by the value of
[getOption](https://rdrr.io/r/base/options.html)`("oceTz")`). Note that
[`subset()`](https://rdrr.io/r/base/subset.html) may be more useful than
this function.

## Usage

``` r
# S3 method for class 'oce'
window(
  x,
  start = NULL,
  end = NULL,
  frequency = NULL,
  deltat = NULL,
  extend = FALSE,
  which = c("time", "distance"),
  indexReturn = FALSE,
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- start:

  the start time (or distance) of the time (or space) region of
  interest. This may be a single value or a vector.

- end:

  the end time (or distance) of the time (or space) region of interest.
  This may be a single value or a vector.

- frequency:

  not permitted yet.

- deltat:

  not permitted yet

- extend:

  not permitted yet

- which:

  string containing the name of the quantity on which sampling is done.
  Possibilities are `"time"`, which applies the windowing on the `time`
  entry of the `data` slot, and `"distance"`, for the `distance` entry
  (for those objects, such as `adp`, that have this entry).

- indexReturn:

  boolean flag indicating whether to return a list of the "kept" indices
  for the `time` entry of the `data` slot, as well as the `timeSlow`
  entry, if there is one.. Either of these lists will be `NULL`, if the
  object lacks the relevant items.

- debug:

  a flag that turns on debugging.

- ...:

  ignored

## Value

Normally, this is new `oce` object. However, if `indexReturn=TRUE`, the
return value is two-element list containing items named `index` and
`indexSlow`, which are the indices for the `time` entry of the `data`
slot (and the `timeSlow`, if it exists).

## See also

[`subset()`](https://rdrr.io/r/base/subset.html) provides more flexible
selection of subsets.

## Author

Dan Kelley

## Examples

``` r
library(oce)
data(adp)
plot(adp)

early <- window(adp, start = "2008-06-26 00:00:00", end = "2008-06-26 12:00:00")
plot(early)

bottom <- window(adp, start = 0, end = 20, which = "distance")
plot(bottom)
```
