# Look Within the First Element of a List for Replacement Values

This is a helper function used by some seawater functions (with names
starting with `sw`) to facilitate the specification of water properties
either with distinct arguments, or with data stored within an `oce`
object that is the first argument.

## Usage

``` r
lookWithin(list)
```

## Arguments

- list:

  A list of elements, typically arguments that will be used in sw
  functions.

## Value

A list with elements of the same names but possibly filled in from the
first element.

## Details

If `list[1]` is not an `oce` object, then the return value of
`lookWithin` is the same as the input value, except that (a) `eos` is
completed to either `"gsw"` or `"unesco"` and (b) if `longitude` and
`latitude` are within `list[1]`, then they are possibly lengthened, to
have the same length as the first item in the `data` slot of `list[1]`.

The examples may clarify this somewhat.

## Examples

``` r
# 1. If first item is not a CTD object, just return the input
lookWithin(list(a = 1, b = 2)) # returns a list
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
# 2. Extract salinity from a CTD object
data(ctd)
str(lookWithin(list(salinity = ctd)))
#> List of 1
#>  $ salinity: num [1:181] 29.9 29.9 29.9 29.9 29.9 ...
# 3. Extract salinity and temperature. Note that the
# value specified for temperature is ignored; all that matters
# is that temperature is named.
str(lookWithin(list(salinity = ctd, temperature = NULL)))
#> List of 2
#>  $ salinity   : num [1:181] 29.9 29.9 29.9 29.9 29.9 ...
#>  $ temperature: num [1:181] 14.2 14.2 14.2 14.2 14.2 ...
# 4. How it is used by swRho()
rho1 <- swRho(ctd, eos = "unesco")
rho2 <- swRho(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]], eos = "unesco")
stopifnot(all.equal(rho1, rho2))
```
