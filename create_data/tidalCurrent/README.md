Running `make` here creates `tideCurrent.rda` here, and running `make install`
copies it to `../data`.  The work is done with `create_tideCurrent.R`, and the
data files `tide8.dat` and `tide9.dat` are from a zipfile [2] downloaded on
2020 aug 5.  It should be noted that [2] also contains the full text of [1].

The data format is format is described on page 5 (PDF page 12) of [1], and
`create_tidalCurrent.R` uses this as the first step in reading the data, after
which it does some housekeeping tasks to put the data into a usable format as
described in the documentation provided by `?tidalCurrent`.

## References

1. Foreman, M. G. G. “Manual for Tidal Currents Analysis and Prediction.”
   Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
   Sciences, Patricia Bay, 1978.
2. https://www.dfo-mpo.gc.ca/science/documents/data-donnees/tidal-marees/tidpack.zip

