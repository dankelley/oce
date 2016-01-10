# Instructions

1. All datasets should be created here. 
2. Subdirectories should be named to match those datasets.
3. Add an item for the new subdirectory in the first line of the Makefile in the present directory.
4. Copy an existing `Makefile` to newly-added directories, changing the first line as appropriate.
5. Add any new dataset to the various `check*.R` files found here. These check things like
  a. `check_units.R` -- ensure the existence of `units` within `metadata` (becomes moot once it works, late 2015)
  b. `check_summary.R` -- check output of `summary()` is good on all datasets (e.g. are units good?)


