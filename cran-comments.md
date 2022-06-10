# Submission of oce 1.7-6

* Address compiler warning about comparing a signed int with an unsigned int.

* We hope for an exception to the 10-minute rule for this old and large
  package, offering more than 300 functions spanning over 60k lines of R, 6k
  lines of C/C++ and 1k line of Fortran.

# Submission of oce 1.7-5

* Remove a stray file that was included in our failed submission of oce 1.7-4,
  made 2 days ago.

* We hope for an exception to the 10-minute rule for this old and large
  package, offering more than 300 functions spanning over 60k lines of R, 6k
  lines of C/C++ and 1k line of Fortran.

# Submission of oce 1.7-4

* This version (a) changes an "akima" function call with an "interp"
  equivalent, satisfying a CRAN request communicated by B. Ripley on
  2022-06-04, (b) incorporates code and documentation improvements made since
  the previous release and (c) removes a stray file.

* We hope for an exception to the 10-minute rule for this old and large
  package, offering more than 300 functions spanning over 60k lines of R, 6k
  lines of C/C++ and 1k line of Fortran.

# Tests

## Local Tests

Local MacOS-12.5(beta) R-4.2.0 CMD (BUILD, INSTALL, CHECK): no ERRORs, no
WARNINGs, and 2 NOTEs, one naming the author the other reporting that
sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb,
and help 3.8Mb.

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

