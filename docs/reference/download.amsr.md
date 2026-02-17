# Download and Cache an amsr File

If the file is already present in `destdir`, then it is not downloaded
again. The default `destdir` is the present directory, but it probably
makes more sense to use something like `"~/data/amsr"` to make it easy
for scripts in other directories to use the cached data. The file is
downloaded with
[`download.file()`](https://rdrr.io/r/utils/download.file.html). Please
read the “History” section for important details on how
`download.amsr()` and also
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md)
have had be altered over the years, to deal with changes in the
directory structure and file format on the server from which files are
downloaded.

## Usage

``` r
download.amsr(
  year = NULL,
  month,
  day,
  destdir = ".",
  server = "https://data.remss.com/amsr2/ocean/L3/v08.2",
  type = "3day",
  debug = 0
)
```

## Arguments

- year, month, day:

  a specification of the desired observation time. There are 3 choices
  for this specification. (a) If `year` is an object created by
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html), then that
  specifies the time, and so `month` and `day` are ignored. This scheme
  can be convenient for creating a sequence of images, starting at a
  particular date, because adding 1 to an object of class `Date`
  increases the time by 1 day, saving the user from having to know how
  many days are in any given month. (b) If `year` is an integer, then it
  is taken to be the year, and the user must also specify `month` and
  `day`, also integers. (c) If `year` is NULL (which is the default),
  then the focus is set to the most recent date, but this depends on the
  value of `type` (see next). If `type` is `"3day"`, `"daily"` or
  `"weekly"`, or just the first two of them if `type` is `"monthly"`. If
  these things are provided, then they just match exactly the values in
  the sought-after file on the remote server. If `year` is NULL, then
  `download.amsr()` constructs a URL that ought to be the most recent
  available file: 3 days prior to the present date (if `type` is
  `"3day"` or `"daily"`), the Saturday two weeks prior to the present
  date (if `type` is `"weekly"`), or two months in the past (if `type`
  is `"monthly"`).

- destdir:

  A string naming the directory in which to cache the downloaded file.
  The default is to store in the present directory, but many users find
  it more helpful to use something like `"~/data/amsr"` for this, to
  collect all downloaded amsr files in one place.

- server:

  A string naming the server from which data are to be acquired. See
  “History”.

- type:

  character value indicating where to get the data. This may be `"3day"`
  (the default), for a composite covering 3 days of observation, which
  removes most viewing-path and cloud blanks, `"daily"` for a daily
  reading, `"weekly"` for a composite covering a week, `"monthly"` for a
  composite covering a month, or `"rt"` for what seems to be realtime
  datasets (which seem to be restricted to the prior few days only).
  Note that the `"rt"` files store `SST` and related variables in 3D
  arrays, as opposed to the 2D arrays used for the other file types.
  This is because the ascending and descending passes are both
  available; to select a choice for plotting, use the `pass` parameter
  of
  [`plot,amsr-method()`](https://dankelley.github.io/oce/reference/plot-amsr-method.md).

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

`download.amsr` returns a character value holding the full pathname of
the downloaded file.

## History

Until 25 March 2017, the default server was
`"ftp.ssmi.com/amsr2/bmaps_v07.2"`, but this was changed when the author
discovered that this FTP site had been changed to require users to
create accounts to register for downloads. The default was changed to
`"http://data.remss.com/amsr2/bmaps_v07.2"` on the named date. This site
was found by a web search, but it seems to provide proper data. It is
assumed that users will do some checking on the best source.

On 23 January 2018, it was noticed that the server-url naming convention
had changed, e.g.
`http://data.remss.com/amsr2/bmaps_v07.2/y2017/m01/f34_20170114v7.2.gz`
becoming
`http://data.remss.com/amsr2/bmaps_v08/y2017/m01/f34_20170114v8.gz`

On 2023-07-26, it was noticed that the server-url naming convention had
changed again, requiring not only the alteration of the default `server`
value but also the addition of a new parameter named `type`. Worse yet
the file format had evidently been changed from a gzipped format to a
NetCDF format, which required a complete rewriting of
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md).

On 2024-08-17, it was noticed that the server has a directory called
`rt` which seems to hold realtime data for a few recent days (and a few
other isolated 3-day sequences in the past two years). These files may
be of use in analysis of current events.

## Sample of Usage

    # The download may take up to about a minute.
    f <- download.amsr(2023, 7, 27, destdir="~/data/amsr")
    d <- read.amsr(f)
    plot(d)
    mtext(d[["filename"]], side=3, line=0, adj=0)

## See also

Other functions that download files:
[`download.coastline()`](https://dankelley.github.io/oce/reference/download.coastline.md),
[`download.met()`](https://dankelley.github.io/oce/reference/download.met.md),
[`download.topo()`](https://dankelley.github.io/oce/reference/download.topo.md)

Other things related to amsr data:
[`[[,amsr-method`](https://dankelley.github.io/oce/reference/sub-sub-amsr-method.md),
`[[<-,amsr-method`,
[`amsr`](https://dankelley.github.io/oce/reference/amsr.md),
[`amsr-class`](https://dankelley.github.io/oce/reference/amsr-class.md),
[`composite,amsr-method`](https://dankelley.github.io/oce/reference/composite-amsr-method.md),
[`plot,amsr-method`](https://dankelley.github.io/oce/reference/plot-amsr-method.md),
[`read.amsr()`](https://dankelley.github.io/oce/reference/read.amsr.md),
[`subset,amsr-method`](https://dankelley.github.io/oce/reference/subset-amsr-method.md),
[`summary,amsr-method`](https://dankelley.github.io/oce/reference/summary-amsr-method.md)

## Author

Dan Kelley
