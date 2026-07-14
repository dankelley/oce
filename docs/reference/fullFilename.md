# Full Name of File, Including Path

Determines the full name of a file, including the path. Used by many
`read.X` routines, where `X` is the name of a class of object. This is a
wrapper around
[`normalizePath()`](https://rdrr.io/r/base/normalizePath.html), with
warnings turned off so that messages are not printed for files that are
not found (e.g. URLs).

## Usage

``` r
fullFilename(filename)
```

## Arguments

- filename:

  name of file

## Value

Full file name

## Author

Dan Kelley
