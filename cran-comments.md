# Tests

## Local Tests

Local MacOS-11.1 R-4.0.3 CMD (BUILD, INSTALL, CHECK): no ERRORs or WARNINGs but
the usual note on the author, plus another NOTE on sub-directories of 1MB or
more: (R 3.0Mb, data 1.2Mb, doc 2.3Mb, help 2.8Mb).

## Github R-CMD-check Action Tests

R-CMD-check github action reports no problems on
* windows-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)

However, it reports **failure** on macOS-latest (release), owing R-CMD-check
having a difficulty (not related to `oce`) in locating gfortran. I reported
this as an issue, and in short order a pull request
(https://github.com/r-lib/actions/pull/232) was proposed as a solution.  This
PR has not yet been merged into R-CMD-check, but there is reason to expect
clean results for `oce` when it is merged, given that there are no problems on
my own macOS machine, or in R-hub tests.


## Remote Windows Checks

Using
```R
devtools:::check_win()
```
yields clean results, with just the usual NOTE about the author.

# Reverse Dependency Checks

Using
```
devtools::install_github("r-lib/revdepcheck")
revdep_check(timeout=30*60,num_workers=4)
```
yields
```
── CHECK ───────────────────────────────────────────────── 8 packages ──
✓ oceanwaves 0.1.0                       ── E: 0     | W: 0     | N: 0    
✓ seacarb 3.2.15                         ── E: 0     | W: 0     | N: 0    
✓ graticule 0.1.2                        ── E: 0     | W: 0     | N: 0    
✓ dendroTools 1.1.1                      ── E: 0     | W: 0     | N: 0    
✓ morphomap 1.3                          ── E: 0     | W: 0     | N: 0    
✓ soundecology 1.3.3                     ── E: 0     | W: 0     | N: 0    
✓ SWMPr 2.4.0                            ── E: 0     | W: 0     | N: 0    
✓ vprr 0.1.0                             ── E: 0     | W: 0     | N: 2    
OK: 8                                                                 
BROKEN: 0
```

