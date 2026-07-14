# Determine Time Offset From Timezone

The data are from `https://www.timeanddate.com/time/zones/` and were
hand-edited to develop this code, so there may be errors. Also, note
that some of these contradict; if you examine the code, you'll see some
commented-out portions that represent solving conflicting definitions by
choosing the more common timezone abbreviation over a the less common
one.

## Usage

``` r
GMTOffsetFromTz(tz)
```

## Arguments

- tz:

  a timezone, e.g. `UTC`.

## Value

Number of hours in offset, e.g. `AST` yields 4.

## Author

Dan Kelley

## Examples

``` r
library(oce)
cat("Atlantic Standard Time is ", GMTOffsetFromTz("AST"), "hours after UTC")
#> Atlantic Standard Time is  4 hours after UTC
```
