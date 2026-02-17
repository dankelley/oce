# Read an Argo Data File

`read.argo` is used to read an Argo file, producing an
[argo](https://dankelley.github.io/oce/reference/argo-class.md) object.
The file must be in the ARGO-style NetCDF format described in the Argo
documentation (see references 2 and 3).

## Usage

``` r
read.argo(
  file,
  encoding = NA,
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
```

## Arguments

- file:

  A character string giving the name of the file to load.

- encoding:

  ignored.

- debug:

  A flag that turns on debugging. Set to 1 to get a moderate amount of
  debugging information, or to 2 to get more.

- processingLog:

  If provided, the action item to be stored in the log. (Typically only
  provided for internal calls; the default that it provides is better
  for normal calls by a user.)

- ...:

  additional arguments, passed to called routines.

## Value

`read.argo` returns an
[argo](https://dankelley.github.io/oce/reference/argo-class.md) object.

## Details

See the Argo documentation (see references 2 and 3) for some details on
what files contain. Many items listed in section 2.2.3 of reference 3
are read from the file and stored in the `metadata` slot, with the
exception of `longitude` and `latitude`, which are stored in the `data`
slot, alongside hydrographic information. The details of storage in the
return value are somewhat complex, although the following notes might be
helpful to readers seeking to learn more.

*1. Variable renaming.*

The names of several data parameters stored within the NetCDF file are
altered to fit the oce context. For example, `PRES` becomes `pressure`,
matching the name of this variable in other oce data types. The original
names are reported by `summary,argo-method`, and data may be extracted
with `[[,argo-method` using those names, so the renaming should not be
too inconvenient to Argo experts who are new to oce.

Argo NetCDF files employ a `"SNAKE_CASE"` naming scheme (sometimes using
lower case) that is inconsistent with the `"camelCase"` scheme used in
oce. Since argo objects are just a small part of oce, a decision was
made to rename argo items. For example, `"CYCLE_NUMBER"` in the NetCDF
file becomes `"cycleNumber"` in the oce object returned by `read.argo`.
(Note that `[[,argo-method` also accepts `"cycle"` for this item.) The
conversion for objects in the `data` slot often also involves expanding
on argo abbreviations, e.g. `"PSAL"` becomes `"salinity"`. The renaming
work is carried out with
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md)
for handles both name expansion for several dozen special cases, and
with
[`snakeToCamel()`](https://dankelley.github.io/oce/reference/snakeToCamel.md)
with the `specialCase` argument set to `"QC"`. While this results in
variable names that should make sense in the general oce context (where,
for example, salinity is expected to be stored in a variable named
`"salinity"`), it may be confusing to argo experts who are just starting
to use oce. Such people might find it helpful to use e.g.
`sort(names(x[["metadata"]]))` to get a list of all items in the
`metadata` slot (or similar with `"data"`), since working in reverse may
be easier than simply guessing at what names oce has chosen. (Note that
prior to 2020 June 24, some metadata items were stored in
`"SNAKE_CASE"`.)

*2. Metadata.*

Several of the NetCDF global attributes are also renamed before
placement in the `metadata` slot of the return value. These include
`conventions`, `featureType`, `history`, `institution`, `nParameters`,
`nProfiles`, `references`, `source`, `title`, and `userManualVersion`.
These names are derived from those in the NetCDF file, and mainly follow
the pattern explained in the “Variable renaming convention” section.

For profile data (as indicated by the NetCDF global attribute named
`"featureType"` being equal to `"trajectoryProfile"`), the NetCDF item
named `"STATION_PARAMETERS"` controls whether variables in the source
file will be stored in the `metadata` or `data` slot of the returned
object. If `STATION_PARAMETERS` is not present, as is the case for
trajectory files (which are detected by `featureType` being
`"trajectory"`), some guesses are made as to what goes in `data` and
`metadata` slots.

*3. Data variants.*

Each data item can have variants, as described in Sections 2.3.4 of
reference 3. For example, if `"PRES"` is found in `STATION_PARAMETERS`,
then `PRES` (pressure) data are sought in the file, along with
`PRES_QC`, `PRES_ADJUSTED`, `PRES_ADJUSTED_QC`, and `PRES_ERROR`. The
same pattern works for other profile data. The variables are stored with
names created as explained in the “Variable renaming convention” section
below. Note that flags, which are stored variables ending in `"_QC"` in
the NetCDF file, are stored in the `flags` item within the `metadata`
slot of the returned object; thus, for example, `PRES_QC` is stored as
`pressure` in `flags`.

*4. How time is handled.*

The NetCDF files for profile data store time in an item named `juld`,
which holds the overall profile time, in what the Argo documentation
calls Julian days, with respect to a reference time that is also stored
in the file. Based on this information, a
[POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) value named
`time` is stored in the `metadata` slot of the returned value, and this
may be found with e.g. `a[["time"]]`, where `a` is that returned value.
Importantly, this value matches the time listed in profile index files.
In addition, some profile data files contain a field called `MTIME`,
which holds the offset (in days) between the time of individual
measurements and the overall profile time. For such files, the
measurement times may be computed with `a[["time"]]+86400*a[["mtime"]]`.
(This formula is used by
[`as.ctd()`](https://dankelley.github.io/oce/reference/as.ctd.md), if
its first argument is an
[argo](https://dankelley.github.io/oce/reference/argo-class.md) object
created by supplying `read.argo()` with such a data file.)

*5. Data sources.*

Argo data are made available at several websites. A bit of detective
work can be required to track down the data.

Some servers provide data for floats that surfaced in a given ocean on a
given day, the anonymous FTP server `usgodae.org/pub/outgoing/argo/geo/`
being an example.

Other servers provide data on a per-float basis. A complicating factor
is that these data tend to be categorized by "dac" (data archiving
centre), which makes it difficult to find a particular float. For
example, `https://www.usgodae.org/ftp/outgoing/argo/` is the top level
of a such a repository. If the ID of a float is known but not the "dac",
then a first step is to download the text file
`https://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt` and
search for the ID. The first few lines of that file are header, and
after that the format is simple, with columns separated by slash (`/`).
The dac is in the first such column and the float ID in the second. A
simple search will reveal the dac. For example `data(argo)` is based on
float 6900388, and the line containing that token is
`bodc/6900388/6900388_meta.nc,846,BO,20120225005617`, from which the dac
is seen to be the British Oceanographic Data Centre (`bodc`). Armed with
that information, visit
`https://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388` and see a
directory called `profiles` that contains a NetCDF file for each profile
the float made. These can be read with `read.argo`. It is also possible,
and probably more common, to read a NetCDF file containing all the
profiles together and for that purpose the file
`https://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc`
should be downloaded and provided as the `file` argument to `read.argo`.
This can be automated as in Example 2, although readers are cautioned
that URL structures tend to change over time.

Similar steps can be followed on other servers.

## Sample of Usage

    # Example 1: read from a local file
    library(oce)
    d <- read.argo("~/data/OAR/6900388_prof.nc")
    summary(d)
    plot(d)

    # Example 2: construct URL for download (brittle)
    id <- "6900388"
    url <- "https://www.usgodae.org/ftp/outgoing/argo"
    if (!length(list.files(pattern="argo_index.txt")))
        download.file(paste(url, "ar_index_global_meta.txt", sep="/"), "argo_index.txt")
    index <- readLines("argo_index.txt")
    line <- grep(id, index)
    if (0 == length(line))
        stop("id ", id, " not found")
    if (1 < length(line))
        stop("id ", id, " found multiple times")
    dac <- strsplit(index[line], "/")[[1]][1]
    profile <- paste(id, "_prof.nc", sep="")
    float <- paste(url, "dac", dac, id, profile, sep="/")
    download.file(float, profile)
    argo <- read.argo(profile)
    summary(argo)

## References

1.  `https://argo.ucsd.edu`

2.  Argo User's Manual Version 3.2, Dec 29th, 2015, available at
    `https://archimer.ifremer.fr/doc/00187/29825/` online.

3.  User's Manual (ar-um-02-01) 13 July 2010, available at
    `http://www.argodatamgt.org/content/download/4729/34634/file/argo-dm-user-manual-version-2.3.pdf`,
    which is the main document describing argo data.

## See also

The documentation for the
[argo](https://dankelley.github.io/oce/reference/argo-class.md) class
explains the structure of argo objects, and also outlines the other
functions dealing with them.

Other things related to argo data:
[`D4902337_219.nc`](https://dankelley.github.io/oce/reference/D4902337_219.nc.md),
[`[[,argo-method`](https://dankelley.github.io/oce/reference/sub-sub-argo-method.md),
`[[<-,argo-method`,
[`argo`](https://dankelley.github.io/oce/reference/argo.md),
[`argo-class`](https://dankelley.github.io/oce/reference/argo-class.md),
[`argo2ctd()`](https://dankelley.github.io/oce/reference/argo2ctd.md),
[`argoGrid()`](https://dankelley.github.io/oce/reference/argoGrid.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`as.argo()`](https://dankelley.github.io/oce/reference/as.argo.md),
[`handleFlags,argo-method`](https://dankelley.github.io/oce/reference/handleFlags-argo-method.md),
[`plot,argo-method`](https://dankelley.github.io/oce/reference/plot-argo-method.md),
[`read.argo.copernicus()`](https://dankelley.github.io/oce/reference/read.argo.copernicus.md),
[`subset,argo-method`](https://dankelley.github.io/oce/reference/subset-argo-method.md),
[`summary,argo-method`](https://dankelley.github.io/oce/reference/summary-argo-method.md)

## Author

Dan Kelley
