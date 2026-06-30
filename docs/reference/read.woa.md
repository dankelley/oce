# Read a World Ocean Atlas NetCDF File

Read a World Ocean Atlas NetCDF File

## Usage

``` r
read.woa(file, name, positive = FALSE, encoding = NA)
```

## Arguments

- file:

  character string naming the file

- name:

  of variable to extract. If not provided, an error message is issued
  that lists the names of data in the file.

- positive:

  logical value indicating whether `longitude` should be converted to be
  in the range from 0 to 360, with `name` being shuffled accordingly.
  This is set to `FALSE` by default, because the usual oce convention is
  for longitude to range between -180 to +180.

- encoding:

  ignored.

## Value

A list containing vectors `longitude`, `latitude`, `depth`, and an array
with the specified name. If `positive` is true, then `longitude` will be
converted to range from 0 to 360, and the array will be shuffled
accordingly.

## Sample of Usage


    # Mean SST at 5-degree spatial resolution
    tmn <- read.woa("~/data/woa13/woa13_decav_t00_5dv2.nc", "t_mn")
    imagep(tmn$longitude, tmn$latitude, tmn$t_mn[, , 1], zlab="SST")
