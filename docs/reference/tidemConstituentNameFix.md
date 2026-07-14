# Change Tidal Constituent Name from T-TIDE to Foreman Convention

This is used by
[`tidem()`](https://dankelley.github.io/oce/reference/tidem.md) to
permit users to specify constituent names in either the T-TIDE
convention (see Pawlowicz et al. 2002) or Foreman convention (see
Foreman (1978). There are only two such instances: `"MS"`, which gets
translated to `"M8"`, and `"UPSI"`, which gets translated to `"UPS1"`.

## Usage

``` r
tidemConstituentNameFix(names, debug = 1)
```

## Arguments

- names:

  a vector of character values, holding constituent names

- debug:

  an integer controlling warnings. If this is zero, then no warnings are
  issued during processing; otherwise, as is the default, warnings are
  issued for each conversion that is required.

## Value

A vector of character values of tidal constituent names, in the Foreman
naming convention.

## References

Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and
Prediction. Pacific Marine Science Report. British Columbia, Canada:
Institute of Ocean Sciences, Patricia Bay.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal
harmonic analysis including error estimates in MATLAB using `T_TIDE`.
Computers and Geosciences, 28, 929-937.
