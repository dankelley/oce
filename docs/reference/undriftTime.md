# Correct for Drift in an Instrument Clock

It is assumed that the instrument clock matches the real time at the
start of the sampling, and that the clock drifts linearly (i.e. is
uniformly fast or slow) over the sampling interval. Linear interpolation
is used to infer the values of all variables in the `data` slot. The
data length is altered in this process, e.g. a slow instrument clock
(positive `slowEnd`) takes too few samples in a given time interval, so
`undriftTime` will increase the number of data.

## Usage

``` r
undriftTime(x, slowEnd = 0, tname = "time")
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- slowEnd:

  number of seconds to add to final instrument time, to get the correct
  time of the final sample. This will be a positive number, for a "slow"
  instrument clock.

- tname:

  Character string indicating the name of the time column in the `data`
  slot of `x`.

## Value

An object of the same class as `x`, with the `data` slot adjusted
appropriately.

## Sample of Usage

    library(oce)
    file <- "~/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/raw/pt_rbr_011855.dat"
    rbr011855 <- read.oce(file)
    d <- subset(rbr011855, time < as.POSIXct("2008-06-25 10:05:00"))
    x <- undriftTime(d, 1)   # clock lost 1 second over whole experiment
    summary(d)
    summary(x)

## Author

Dan Kelley
