# Submission of oce 1.8-3

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

The `NEWS.md` file lists 30 changes made since the last CRAN release, all aimed
at improving oce by removing bugs and adding functionality.

## Local Tests

R 4.4.1 on macOS Beta 15.0 Beta (24A5298h) revealed no ERRORs, no WARNINGs, and
2 NOTEs. One NOTE was about the author name and the other about the package
size (a known issue; see above).  These tests included those in the CRAN test
suite, along with other tests of datasets that are not provided with the
package.

In addition to the test suite, the following local tests passed without
problems.

```R
devtools::spell_check()
urlchecker::url_check()
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)
```

## Remote Checks via Github Actions

1. The `R-CMD-check` github action passed without errors.
2. The `R-hub` github action passed on linux (R-devel) and macos (R-devel), but
   failed on windows (R-devel), owing to a problem with this last machine
   building the `ncdf4` package.  As this problem with building `ncdf4` is
   unrelated to `oce`, it was not considered a reason to avoid submission of
   `oce` to CRAN.

## Remote Windows Checks

No errors were detected from the following tests.

```R
devtools::check_mac_release()
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
