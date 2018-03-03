## Test environments

* local R-3.4.3 MacOS install OK
* devtools::build_win(version="R-release") # OK
* devtools::build_win(version="R-devel") # 'Failed FTP upload: 550'
* rhub::check_for_cran() # OK
* rhub::check_with_rdevel() # OK

## R CMD check results

0 errors | 0 warnings | 1 note

## Reverse dependencies

devtools::revdep_check() reports as follows


---


