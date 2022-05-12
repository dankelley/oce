# Submission of 1.7-4

I am submitting oce 1.7-4 even though I fear it will exceed the 10-min limit
that caused rejection of the 1.7-3 version that was rejected yesterday. In the
following, I sketch the efforts I've made to speed things up, the difficulty of
meeting the limit for such a large package, and my hope that an exception can
be made to the rule.

Version 1.7-3 took 13 min on the CRAN check machine. To try to speed things up,
I tried three things: (A) eliminating the test suite for CRAN builds (whilst
retaining it in builds from the github source); (B) shortening several examples
in the documentation and (C) more radically, removing the vignettes. I used
devtools::check_win_devel() as a tool to gauge speedup. The results were as
follows.

* A and B: Before making these changes, check_win_devel() reported 18.3 min
  elapsed time.  After making these changes, the time dropped to 17.5 min. The
  speedup amounts to only 4.4 percent, clearly insufficient to meet the
  requirement. Testing suggested a speedup of 9.3 percent might result from
  removing *all* the example code in the documentation, but even this extreme
  measure would likely yield 11.9 min on the CRAN machine.

* C: After additionally removing the vignettes, the check_win_devel() time was
  reduced to 10.3 min, which amounts to a 44 percent reduction compared to the
  original.  This suggests that even a partial thinning of the vignettes might
  get the package under the 10-min limit.

OLD OLD OLD Based on the above, it seems to me that the oce package cannot fulfill the
10-min requirement. It is, after all, quite a big package, with 68k lines in
R to be examined (and used to build of 54k lines of Rd), in addition to 8k
lines of C, C++ and Fortran to be compiled.

I wonder, would it possible to get an exception to the 10-min rule?

# Tests

## Local Tests

Local MacOS-12.4(beta 21F5071b) R-4.2.0 CMD (BUILD, INSTALL, CHECK): no ERRORs,
no WARNINGs, and 2 NOTEs, one naming the author the other reporting that
sub-directories were of size of 1MB or more: R 3.0Mb, data 1.0Mb, doc 2.0Mb,
and help 3.6Mb.

## Github R-CMD-check Action Tests

R-CMD-check github action reports no problems on
* windows-latest (release)
* macOS-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)


## Remote Windows Checks

Using
```R
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
identified no errors.

# Reverse Dependency Checks

Using
```
# devtools::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(quiet=FALSE)
```
identified no errors and no warnings.

