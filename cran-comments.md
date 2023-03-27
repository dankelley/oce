# Submission of oce 1.7-11

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

The main reason for the submission is a request to remove dependence on rgeos
and raster packages, which are being superseded by newer packages.

The NEWS.md file also lists over a dozen improvements.  Of these, two have
user-visible changes.  First, plot() for echosounder-class objects now uses the
viridis colourscheme by default (it was jet, previously). Second, and more
significantly, AD2CP objects now have a layout that matches other oce objects,
simplifying the analysis of such data.

## Local Tests

Local MacOS-13(Ventura 13.3) with R 4.2.3: no ERRORs, no WARNINGs, and 2 NOTEs,
one naming the author the other reporting that sub-directories were of size of
1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb, and help 3.8Mb.

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


## Other checks

No problems were found with any of the following
```
devtools::spell_check()
urlchecker::url_check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_win_oldrelease()
rhub::check_for_cran(email="Dan.Kelley@Dal.Ca", show_status=FALSE)
rhub::check(platform="debian-clang-devel", show_status=FALSE)
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers=4)
```
