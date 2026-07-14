# Oce Version of axis.POSIXct

A specialized variant of
[`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html) that
produces results with less ambiguity in axis labels.

## Usage

``` r
oce.axis.POSIXct(
  side,
  x,
  at,
  tformat,
  labels = TRUE,
  drawTimeRange,
  abbreviateTimeRange = FALSE,
  drawFrequency = FALSE,
  cex.axis = par("cex.axis"),
  cex.lab = par("cex.lab"),
  cex.main = par("cex.main"),
  mar = par("mar"),
  mgp = par("mgp"),
  main = "",
  debug = getOption("oceDebug"),
  ...
)
```

## Arguments

- side:

  as for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html).

- x:

  as for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html).

- at:

  as for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html).

- tformat:

  as `format` for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html) for
  now, but may eventually have new features for multiline labels, e.g.
  day on one line and month on another.

- labels:

  as for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html).

- drawTimeRange:

  Optional indication of whether/how to draw the time range in the
  margin on the side of the the plot opposite the time axis. If this is
  not supplied, it defaults to the value returned by
  [getOption](https://rdrr.io/r/base/options.html)`("oceDrawTimeRange")`,
  and if that option is not set, it defaults to `TRUE`. No time range is
  drawn if `drawTimeRange` is `FALSE`. If it is `TRUE`, the range will
  be shown. This range refers to range of the x axis (not the data). The
  format of the elements of that range is set by
  [getOption](https://rdrr.io/r/base/options.html)`("oceTimeFormat")`
  (or with the default value of an empty string, if this option has not
  been set). The timezone will be indicated if the time range is under a
  week. For preliminary work, it makes sense to use
  `drawTimeRange=TRUE`, but for published work it can be better to drop
  this label and indicate something about the time in the figure
  caption.

- abbreviateTimeRange:

  boolean, `TRUE` to abbreviate the second number in the time range,
  e.g. dropping the year if it is the same in the first number.

- drawFrequency:

  boolean, `TRUE` to show the frequency of sampling in the data

- cex.axis, cex.lab, cex.main:

  character expansion factors for axis numbers, axis names and plot
  titles; see [`par()`](https://rdrr.io/r/graphics/par.html).

- mar:

  value for `par(mar)` for axis

- mgp:

  value for `par(mgp)` for axis

- main:

  title of plot

- debug:

  a flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- ...:

  as for
  [`axis.POSIXct()`](https://rdrr.io/r/graphics/axis.POSIXct.html).

## Value

A vector of times corresponding to axis ticks is returned silently.

## Details

The tick marks are set automatically based on examination of the time
range on the axis. The scheme was devised by constructing test cases
with a typical plot size and font size, and over a wide range of time
scales. In some categories, both small tick marks are interspersed
between large ones.

The user may set the format of axis numbers with the `tformat` argument.
If this is not supplied, the format is set based on the time span of the
axis:

- If this time span is less than a minute, the time axis labels are in
  seconds (fractional seconds, if the interval is less than 2 seconds),
  with leading zeros on small integers. (Fractional seconds are enabled
  with a trick: the usual R format `"\%S"` is supplemented with a new
  format e.g. `"\%.2S"`, meaning to use two digits after the decimal.)

- If the time span exceeds a minute but is less than 1.5 days, the label
  format is `"\%H:\%M:\%S"`.

- If the time span exceeds 1.5 days but is less than 1 year, the format
  is `"\%b \%d"` (e.g. Jul 15) and, again, the tick marks are set up for
  several subcategories.

- If the time span exceeds a year, the format is `"\%Y"`, i.e. the year
  is displayed with 4 digits.

It should be noted that this scheme differs from the R approach in
several ways. First, R writes day names for some time ranges, in a
convention that is seldom seen in the literature. Second, R will write
nn:mm for both HH:MM and MM:SS, an ambiguity that might confuse readers.
Third, the use of both large and small tick marks is not something that
R does.

Bear in mind that `tformat` may be set to alter the number format, but
that the tick mark scheme cannot (presently) be controlled.

## See also

This is used mainly by
[`oce.plot.ts()`](https://dankelley.github.io/oce/reference/oce.plot.ts.md).

## Author

Dan Kelley
