## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

* I have run R CMD check on the downstream dependencies.  All pass except for
  'soundecology', but I cannot build 'soundecology' on my osx machine, because
I get an error with building the 'seewave' package (which I assume is the same
error as reported on the CRAN check results webpage for 'soundecology'). 

