# Submission of 1.5-0

This resubmission (a) aligns oce with the Rcpp that Dirk Eddelbuettel plans to
submit to CRAN next week and (b) incorporates 8 months of oce improvements and
bug fixes.

# Tests

## Local Tests

Local MacOS-12.2(beta) R-4.1.2 CMD (BUILD, INSTALL, CHECK): no ERRORs or
WARNINGs but the usual note naming the author, plus another NOTE on
sub-directories of 1MB or more: (R 3.0Mb, data 1.0Mb, doc 2.0Mb, help 3.7Mb).

## Github R-CMD-check Action Tests

R-CMD-check github action reports no problems on
* windows-latest (release)
* macOS-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)


## Remote Windows Checks

Using
```R
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
identified no errors.

# Reverse Dependency Checks

Using
```
# devtools::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(quiet=FALSE)
```
identified no errors.


