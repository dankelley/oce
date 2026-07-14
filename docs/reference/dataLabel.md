# Associate Data Names With Units

Note that the whole object is not being given as an argument; possibly
this will reduce copying and thus storage impact.

## Usage

``` r
dataLabel(names, units)
```

## Arguments

- names:

  the names of data within an object

- units:

  the units from metadata

## Value

a vector of strings, with blank entries for data with unknown units
