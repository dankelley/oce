# Concatenate oce Objects (oce-Specific)

This function concatenates oce objects. It is intended for objects
holding data sampled through time, and it works by pasting together data
linearly if they are vectors, by row if they are matrices, and by second
index if they are arrays. It has been tested for the following classes:
[adp](https://dankelley.github.io/oce/reference/adp-class.md),
[adv](https://dankelley.github.io/oce/reference/adv-class.md),
[ctd](https://dankelley.github.io/oce/reference/ctd-class.md), and
[met](https://dankelley.github.io/oce/reference/met-class.md). It may do
useful things for other classes, and so users are encouraged to try, and
to report problems to the developers. It is unlikely that the function
will do anything even remotely useful for image and topographic data, to
name just two cases that do not fit the sampled-over-time category.

## Usage

``` r
# S4 method for class 'oce'
concatenate(object, ..., debug = getOption("oceDebug"))
```

## Arguments

- object:

  An object of
  [oce](https://dankelley.github.io/oce/reference/oce-class.md), or a
  list containing such objects (in which case the remaining arguments
  are ignored).

- ...:

  optional additional objects of
  [oce](https://dankelley.github.io/oce/reference/oce-class.md).

- debug:

  integer indicating debugging level. If this exceeds 1, some
  information may be printed during the processing.

## Value

An object of
[oce](https://dankelley.github.io/oce/reference/oce-class.md).

## See also

Other functions that concatenate oce objects:
[`concatenate,adp-method`](https://dankelley.github.io/oce/reference/concatenate-adp-method.md)

## Author

Dan Kelley

## Examples

``` r
## 1. Split, then recombine, a ctd object.
data(ctd)
ctd1 <- subset(ctd, scan <= median(ctd[["scan"]]))
ctd2 <- subset(ctd, scan > median(ctd[["scan"]]))
CTD <- concatenate(ctd1, ctd2)

## 2. Split, then recombine, an adp object.
data(adp)
midtime <- median(adp[["time"]])
adp1 <- subset(adp, time <= midtime)
adp2 <- subset(adp, time > midtime)
ADP <- concatenate(adp1, adp2)

if (FALSE) { # \dontrun{
## 3. Download two met files and combine them.
met1 <- read.met(download.met(id=6358, year=2003, month=8))
met2 <- read.met(download.met(id=6358, year=2003, month=9))
MET <- concatenate(met1, met2)
} # }
```
