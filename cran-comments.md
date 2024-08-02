# Submission of oce 1.8-3

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

The NEWS.md file lists 29 changes made in the past 8 months, aimed at improving
oce by removing bugs and adding functionality.

## Local Tests

R 4.4.0 on macOS Beta 15.0 Beta (24A5298h) revealed no ERRORs, no WARNINGs, and
2 NOTEs, one about the author name and the other about the package size.

The following local tests passed without problems.

```R
devtools::spell_check()
urlchecker::url_check()
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)
```

## Remote Windows Checks

No errors were detected from the following tests.

```R
devtools::check_mac_release()
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
# did we do rhub checks?  They fail a lot, for reasons unconnected with oce.
rhub::check_for_cran(email="Dan.Kelley@Dal.Ca", show_status=FALSE)
```
