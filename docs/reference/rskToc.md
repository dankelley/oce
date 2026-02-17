# Decode Table-of-Contents From an rsk File

Decode table-of-contents file from a rsk file, of the sort used by some
researchers at Dalhousie University.

## Usage

``` r
rskToc(dir, from, to, debug = getOption("oceDebug"))
```

## Arguments

- dir:

  name of a directory containing a single table-of-contents file, with
  `.TBL` at the end of its file name.

- from:

  optional [`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html)
  time, indicating the beginning of a data interval of interest. This
  must have timezone `"UTC"`.

- to:

  optional [`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html)
  time, indicating the end of a data interval of interest. This must
  have timezone `"UTC"`.

- debug:

  optional integer to control debugging, with positive values indicating
  to print information about the processing.

## Value

A list with two elements: `filename`, a vector of file names, and
`startTime`, a vector of
[`POSIXct()`](https://rdrr.io/r/base/DateTimeClasses.html) times
indicating the (real) times of the first datum in the corresponding
files.

## Details

It is assumed that the `.TBL` file contains lines of the form
`"File \day179\SL08A179.023 started at Fri Jun 27 22:00:00 2008"` The
first step is to parse these lines to get day and hour information, i.e.
179 and 023 in the line above. Then, recognizing that it is common to
change the names of such files, the rest of the file-name information in
the line is ignored, and instead a new file name is constructed based on
the data files that are found in the directory. (In other words, the
`"\\day179\\SL08A"` portion of the line is replaced.) Once the file list
is complete, with all times put into R format, then (optionally) the
list is trimmed to the time interval indicated by `from` and `to`. It is
important that `from` and `to` be in the `UTC` time zone, because that
time zone is used in decoding the lines in the `.TBL` file.

## Sample of Usage

    file <- "~/data/archive/sleiwex/2008/moorings/m05/adv/sontek_202h/raw"
    table <- rskToc(file,
        from=as.POSIXct("2008-07-01 00:00:00", tz="UTC"),
        to=as.POSIXct("2008-07-01 12:00:00", tz="UTC"))
    print(table)

## See also

Other things related to rsk data:
[`[[,rsk-method`](https://dankelley.github.io/oce/reference/sub-sub-rsk-method.md),
`[[<-,rsk-method`,
[`as.rsk()`](https://dankelley.github.io/oce/reference/as.rsk.md),
[`ctdFindProfilesRBR()`](https://dankelley.github.io/oce/reference/ctdFindProfilesRBR.md),
[`plot,rsk-method`](https://dankelley.github.io/oce/reference/plot-rsk-method.md),
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md),
[`rsk`](https://dankelley.github.io/oce/reference/rsk.md),
[`rsk-class`](https://dankelley.github.io/oce/reference/rsk-class.md),
[`rskPatm()`](https://dankelley.github.io/oce/reference/rskPatm.md),
[`subset,rsk-method`](https://dankelley.github.io/oce/reference/subset-rsk-method.md),
[`summary,rsk-method`](https://dankelley.github.io/oce/reference/summary-rsk-method.md)

## Author

Dan Kelley
