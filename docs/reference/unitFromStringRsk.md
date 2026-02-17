# Infer rsk Units From a Vector of Strings

This is used by
[`read.rsk()`](https://dankelley.github.io/oce/reference/read.rsk.md) to
infer the units of data, based on strings stored in `.rsk` files.
Lacking a definitive guide to the format of these file, this function
was based on visual inspection of the data contained within a few sample
files; unusual sensors may not be handled properly.

## Usage

``` r
unitFromStringRsk(s)
```

## Arguments

- s:

  Vector of character strings, holding the `units` entry in the
  `channels` table of the `.rsk` database.

## Value

List of unit lists.

## See also

Other functions that interpret variable names and units from headers:
[`ODFNames2oceNames()`](https://dankelley.github.io/oce/reference/ODFNames2oceNames.md),
[`cnvName2oceName()`](https://dankelley.github.io/oce/reference/cnvName2oceName.md),
[`oceNames2whpNames()`](https://dankelley.github.io/oce/reference/oceNames2whpNames.md),
[`oceUnits2whpUnits()`](https://dankelley.github.io/oce/reference/oceUnits2whpUnits.md),
[`unitFromString()`](https://dankelley.github.io/oce/reference/unitFromString.md),
[`woceNames2oceNames()`](https://dankelley.github.io/oce/reference/woceNames2oceNames.md),
[`woceUnit2oceUnit()`](https://dankelley.github.io/oce/reference/woceUnit2oceUnit.md)
