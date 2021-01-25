## Test environments

* local MacOS-11.1 R-4.0.3 CMD (BUILD, INSTALL, CHECK): no ERRORs or WARNINGs,
  but the usual note on the author, plus another NOTE on sub-directories of 1MB
  or more: (R 3.0Mb, data 1.2Mb, doc 2.3Mb, help 2.8Mb).
    
* R-CMD-check github action clean on windows-latest (release), ubutntu-20.04
  (release and devel), but failure on macOS-latest (release) owing to general
  problems with that action relating to the connection with gfortran (see
  https://github.com/r-lib/actions/pull/232 for what may be a solution to that
  general problem).

## devtools::revdep_check() results

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

