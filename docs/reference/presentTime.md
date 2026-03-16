# Get the Present Time, in a Stated Timezone

Get the Present Time, in a Stated Timezone

## Usage

``` r
presentTime(tz = "UTC")
```

## Arguments

- tz:

  String indicating the desired timezone. The default is to use UTC,
  which is used commonly in oceanographic work. To get the local time,
  use `tz=""` or `tz=NULL`, as in “Examples”.

## Value

A [`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html)-style
object holding the present time, in the indicated timezone.

## Examples

``` r
presentTime() # UTC
#> [1] "2026-03-16 12:26:33 UTC"
presentTime("") # the local timezone
#> [1] "2026-03-16 09:26:33 ADT"
```
