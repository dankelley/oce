# Submission of oce 1.7-9

As in previous versions, we request an exception to the 10-minute rule, on
account of the package size (300+ functions, covering 60k lines of R, 6k lines
of C/C++ and 1k line of Fortran). The changes from the previous version are as
follows.

* Prepare for the upcoming R release by eliminating `\x` assemblages in
  `grep()` and `gsub()` calls, and adding an `encoding` parameter to the oce
  `read` functions. We thank Tomas Kalibera and Kurt Hornik for helping with
  this, and doing so with remarkable patience.

* Try to address issues reported on the CRAN machine named
  `r-devel-linux-x86_64-debian-clang`. This was tested using
  `rhub::check(platform="debian-clang-devel")`, which `rhub::platforms()`
  summarizes with `Debian Linux, R-devel, clang, ISO-8859-15 locale`.

* Address formatting problems in two generated Rd files (`swTFreeze.html` and
  `swSpecificHeat.Rd`) reported on some CRAN test systems.


# Tests

## Local Tests

Local MacOS-13(Ventura beta) with R 4.2.1 and also R Under development
(unstable) (2022-06-27 r82528): no ERRORs, no WARNINGs, and 2 NOTEs, one naming
the author the other reporting that sub-directories were of size of 1MB or
more: R 3.0Mb, data 1.0Mb, doc 2.0Mb, and help 3.8Mb.

## Remote Windows Checks

Using
```R
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
identified no errors.

## Remote rhub Checks

Using
```R
rhub::check_for_cran()
```
produced no errors relating to oce.


# Reverse Dependency Checks

Using
```
# devtools::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(quiet=FALSE)
```
identified no errors and no warnings.

