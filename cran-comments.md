# Submission of 1.6-0

This submission solves a problem in which a figure referred to an image size
with `width=400px` instead of `width="400"`.  Thanks to Kurt Hornik, for
checking on this across the CRAN package list (and letting maintainers know on
2022-02-17).

# Tests

## Local Tests

Local MacOS-12.2(beta) R-4.1.2 CMD (BUILD, INSTALL, CHECK): no ERRORs, no
WARNINGs, and 2 NOTEs, one naming the author the other reporting that
sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb,
and help 3.7Mb.

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
identified no errors and no warnings.

