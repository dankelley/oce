## Test environments

* local R-3.4.3 MacOS install OK
* devtools::build_win(version="R-release") # OK
* devtools::build_win(version="R-devel") # 'Failed FTP upload: 550'
* rhub::check_for_cran() # OK
* rhub::check_with_rdevel() # OK

Comment: I've tried the build_win(version="R-devel") a few times, without
success, but this seems not to be a difficulty owing to oce. Also, the
rhub::check_for_cran() and rhub::check_with_rdevel() results make me somewhat
sanguine about the possibilility of clean CRAN builds.

## R CMD check results

0 errors | 0 warnings | 1 note

## Reverse dependencies

devtools::revdep_check() reports as follows

* Checked dendroTools : 1 error  | 0 warnings | 0 notes
* Checked graticule   : 0 errors | 0 warnings | 0 notes
* Checked seacarb     : 0 errors | 0 warnings | 0 notes
* Checked skyscapeR   : 0 errors | 0 warnings | 1 note 
* Checked soundecology: 1 error  | 0 warnings | 0 notes
* Checked SWMPr       : 0 errors | 0 warnings | 0 notes

Comment: the error details are as follows

1. dendroTools fails (as noted on its CRAN check page) because it relies on
   RWeka, which is unavailable.

2. seewave fails (as noted on its CRAN check page) because it relies on
   "seawave", which is unavailable.

and so I propose that 'oce' be considered to have passed the dependence checks,
since the listed errors are unrelated to it.


---


This release is partly motivated by a POSIXlt-related error reported in the
R-devel build, which Kurt Hornik kindly pointed out in an email to Dan Kelley,
dated 2018-01-28. I apologize for not monitoring the CRAN check website as
often as I should, to notice new errors.

