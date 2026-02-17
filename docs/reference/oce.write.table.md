# Write the Data Portion of Object to a File

The output has a line containing the names of the columns in `x$data`,
each enclosed in double quotes. After that line are lines for the data
themselves. The default is to separate data items by a single space
character, but this can be altered by using a `sep` argument in the
`...` list; see
[`utils::write.table()`](https://rdrr.io/r/utils/write.table.html).

## Usage

``` r
oce.write.table(x, file = "", ...)
```

## Arguments

- x:

  an [oce](https://dankelley.github.io/oce/reference/oce-class.md)
  object.

- file:

  file name, as passed to
  [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html).
  Use `""` to get a listing in the terminal window.

- ...:

  optional arguments passed to
  [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html).

## Value

The value returned by
[`utils::write.table()`](https://rdrr.io/r/utils/write.table.html).

## Details

This function is little more than a thin wrapper around
[`utils::write.table()`](https://rdrr.io/r/utils/write.table.html), the
only difference being that row names are omitted here, making for a file
format that is more conventional in Oceanography.

## See also

\`[`utils::write.table()`](https://rdrr.io/r/utils/write.table.html),
which does the actual work.

## Author

Dan Kelley
