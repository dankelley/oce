## Test environments
* local R-3.4.3 MacOS install OK
* devtools::build_win(version="R-release") # OK
* devtools::build_win(version="R-devel") # 'Failed FTP upload: 550'
* rhub::check_for_cran() # OK
* rhub:check_with_rdevel() # OK

## R CMD check results

0 errors | 0 warnings | 1 note

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

This release is partly motivated by a POSIXlt-related error reported in the
R-devel build, which Kurt Hornik kindly pointed out in an email to Dan Kelley,
dated 2018-01-28. I apologize for not monitoring the CRAN check website as
often as I should, to notice new errors.

