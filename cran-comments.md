# Submission of oce 1.8-0

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

A key reason for the submission is a request to remove dependence on rgeos and
raster packages, which are being superseded by newer packages.

The NEWS.md file lists over a dozen additional changes.  Of these, two have
user-visible changes.  First, plot() for echosounder-class objects now uses the
viridis colour-scheme by default (it was jet, previously). Second, and more
significantly, AD2CP objects now have a layout that matches other oce objects,
simplifying the analysis of such data.

## Local Tests

R 4.3.0 on MacOS-14 (beta) revealed no ERRORs, no WARNINGs, and 2 NOTEs. One of
the NOTEs is the standard naming of the author, and the other is an indication
of sub-directories over 1MB in size, now as has been the case going back
several years, for this large package.

## Remote Windows Checks

Using
```R
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
identified no errors.

## Other checks

The following checks passed without difficulty, except that some of the RHUB
checks failed for reasons unrelated to the package (owing, it seems, to
difficulties in the setup of the testing machines).

```
devtools::spell_check()
urlchecker::url_check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_win_oldrelease()
rhub::check_for_cran(email="Dan.Kelley@Dal.Ca", show_status=FALSE)
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers=4)
```
