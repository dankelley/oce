# Submission of 1.7-3

* Version 1.7-3 adds new features, and addresses an issue pointed out by Kurt
  Hornik on 2022-05-09 relating to the encoding of a meteorological test file.

# Submission of 1.7-2

Version 1.7-2 fixes a broken link (found upon submitting 1.7-1 to CRAN).

Version 1.7-1 fixes a problem of comparing a `class()` result with a string
(found upon submitting 1.7-0 to CRAN).

Version 1.7-0 adds `read.ctd.aml()` and addresses three problems seen in CRAN
checks of 1.6-0:

1. Fix CRAN check warning about 2 uninitialized variables on
  r-devel-linux-x86_64-debian-gcc.  (We thank K. Hornik, for an email dated
  2022-03-16 notifying us about the problem.)
2. Fix CRAN check error on r-devel-linux-x86_6-debian-clang, -fedora-clang and
  -fedora-gcc, relating to a byte-order-mark in a test file, which is evidently
  treated differently on different systems.
3. Remove a kriging example because it causes problems on some CRAN check
   machines.

# Tests

## Local Tests

Local MacOS-12.2(beta) R-4.1.3 CMD (BUILD, INSTALL, CHECK): no ERRORs, no
WARNINGs, and 2 NOTEs, one naming the author the other reporting that
sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb,
and help 3.6Mb.

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

