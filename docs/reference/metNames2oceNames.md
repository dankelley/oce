# Convert met Data Name to oce Name

Interoperability between oce functions requires that standardized data
names be used, e.g. `"temperature"` for in-situ temperature. Very few
data-file headers name the temperature column in exactly that way,
however, and this function is provided to try to guess the names. The
task is complicated by the fact that Environment Canada seems to change
the names of the columns, e.g. sometimes a symbol is used for the degree
sign, other times not.

## Usage

``` r
metNames2oceNames(names, scheme)
```

## Arguments

- names:

  a vector of character strings with original names

- scheme:

  an optional indication of the scheme that is employed. This may be
  `"ODF"`, in which case
  [`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md)
  is used, or `"met"`, in which case some tentative code for met files
  is used.

## Value

Vector of strings for the decoded names. If an unknown scheme is
provided, this will just be `names`.

## Details

Several quantities in the returned object differ from their values in
the source file. For example, speed is converted from km/h to m/s, and
angles are converted from tens of degrees to degrees. Also, some items
are created from scratch, e.g. `u` and `v`, the eastward and northward
velocity, are computed from speed and direction. (Note that e.g. u is
positive if the wind blows to the east; the data are thus in the normal
Physics convention.)

## See also

Other functions that convert variable names to the oce convention:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`argoNames2oceNames()`](https://dankelley.github.io/oce/reference/argoNames2oceNames.md),
[`bodcNames2oceNames()`](https://dankelley.github.io/oce/reference/bodcNames2oceNames.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md)

## Author

Dan Kelley
