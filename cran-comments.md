# Submission of oce 1.8-2

As in previous CRAN submissions, we request an exception to the 10-minute rule,
on account of the package size (300+ functions, covering 60k lines of R, 6k
lines of C/C++ and 1k line of Fortran).

A key reason for the submission is a requirement to ensure that Rprintf() and
similar functions use properly-matching format codes for the various integer
formats. I was notified that this had become a problem on CRAN machines on Nov
27.

The NEWS.md file lists over additional changes that improve oce by extending
capabilities and removing bugs.

## Local Tests

R 4.3.2 on macOS Sonoma Beta 14.2 (23C5047e) revealed no ERRORs, no WARNINGs,
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
