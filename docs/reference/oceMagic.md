# Find the Type of an Oceanographic Data File

`oceMagic` tries to infer the file type, based on the data within the
file, the file name, or a combination of the two.

## Usage

``` r
oceMagic(file, encoding = "latin1", debug = getOption("oceDebug"))
```

## Arguments

- file:

  a connection or a character string giving the name of the file to be
  checked.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

- debug:

  an integer, set non-zero to turn on debugging. Higher values indicate
  more debugging.

## Value

A character string indicating the file type, or `"unknown"`, if the type
cannot be determined. If the result contains `"/"` characters, these
separate a list describing the file type, with the first element being
the general type, the second element being the manufacturer, and the
third element being the manufacturer's name for the instrument. For
example, `"adp/nortek/aquadopp"` indicates a acoustic-doppler profiler
made by NorTek, of the model type called Aquadopp.

## Details

`oceMagic` was previously called `oce.magic`, but that alias was removed
in version 0.9.24; see
[oce-defunct](https://dankelley.github.io/oce/reference/oce-deprecated.md).

## See also

This is used mainly by
[`read.oce()`](https://dankelley.github.io/oce/reference/read.oce.md).

## Author

Dan Kelley
