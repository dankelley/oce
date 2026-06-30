# Trim an oce File

Create an oce file by copying the first `n` data chunks of another such
file. This can be useful in supplying small sample files for bug
reports. Only a few file types (as inferred with
[`oceMagic()`](https://dankelley.github.io/oce/reference/oceMagic.md))
are permitted.

## Usage

``` r
oceFileTrim(infile, n = 100L, outfile, debug = getOption("oceDebug"))
```

## Arguments

- infile:

  name of an AD2CP source file.

- n:

  integer indicating the number of data chunks to keep. The default is
  to keep 100 chunks, a common good choice for sample files.

- outfile:

  optional name of the new file to be created. If this is not supplied,
  a default is used, by adding `_trimmed` to the base filename, e.g. for
  an AD2CP file named `"a.ad2cp"`, the constructed value of `outfile`
  will be `a_trimmed.ad2cp`.

- debug:

  an integer value indicating the level of debugging. If this is 1L,
  then a brief indication is given of the processing steps. If it is \>
  1L, then information is given about each data chunk, which can yield
  very extensive output.

## Value

`oceFileTrim()` returns the name of the output file, either provided in
the `outfile` parameter or constructed by this function.

## Sample of Usage


    # Can only be run by the developer, since it uses a private file.
    f  <- "~/Dropbox/oce_secret_data/ad2cp/byg_trimmed.ad2cp"
    if (file.exists(f)) {
        oceFileTrim(f, 10L) # this file holds 100 data segments
    }

## See also

Other functions that trim data files:
[`adpAd2cpFileTrim()`](https://dankelley.github.io/oce/reference/adpAd2cpFileTrim.md),
[`adpRdiFileTrim()`](https://dankelley.github.io/oce/reference/adpRdiFileTrim.md),
[`advSontekAdrFileTrim()`](https://dankelley.github.io/oce/reference/advSontekAdrFileTrim.md)

## Author

Dan Kelley
