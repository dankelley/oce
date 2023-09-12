# Submission of oce 1.8-1

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

A key reason for the submission is a requirement to adjust the order of
'include' statements in the C++ code.  (We thank Brian Ripley for explaining the
problem in an email dated 2023-06-26.)

The NEWS.md file lists over ten additional changes, mainly relating to support
for the analysis of measurements of water properties and velocities.

## Local Tests

R 4.3.0 on macOS Ventura (13.5 Beta 22G5048d) revealed no ERRORs, no WARNINGs,
and 2 NOTEs, one about the author name and the other about the package size.

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
