# Submission of oce 1.7-4

This version changes an "akima" function call with an "interp" equivalent,
satisfying a CRAN request communicated by B. Ripley on 2022-06-04.

We request an exemption to the "10 minute rule". Oce is a large package that is
slow to build and test. Even removing the tests and examples might not let us
satisfy this limit, and doing so would significantly reduce the usefulness of
the package to the oceanographic community.

# Tests

## Local Tests

Local MacOS-12.5(beta) R-4.2.0 CMD (BUILD, INSTALL, CHECK): no ERRORs, no
WARNINGs, and 2 NOTEs, one naming the author the other reporting that
sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb,
and help 3.8Mb.

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

