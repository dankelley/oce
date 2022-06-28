# Submission of oce 1.7-7

* Address the need to eliminate \x assemblages in strings used in grep() and
  gsub() calls. Also, set the encoding for input of files that may contain
  problematic characters.  These changes are necessitated by recent changes in
  R-devel, as was pointed out kindly in am email that Tomas Kalibera sent on
  2022-06-27 to me and the maintainers of several other packages.

* As in previous versions of oce, we request an exception to the 10-minute
  rule, since oce is large package (with over 300 functions spanning over 60k
  lines of R, 6k lines of C/C++ and 1k line of Fortran) with a long history on
  CRAN and many users in the oceanographic community.

# Tests

## Local Tests

Local MacOS-12.5(beta) with R Under development (unstable) (2022-06-27 r82528):
no ERRORs, no WARNINGs, and 2 NOTEs, one naming the author the other reporting
that sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc
2.0Mb, and help 3.8Mb.

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
identified no errors with oce, although there are notes about stray files
('lastMiKTeXException') in some instances.


# Reverse Dependency Checks

Using
```
# devtools::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(quiet=FALSE)
```
identified no errors and no warnings.

