## Test environments

* local R-3.6.0 MacOS install OK
* win-builder R-release 3.6.0 (2019-04-26) OK
* win-builder R-devel (unstable) (2019-06-11 r76701) OK
* win-builder R-oldrelease R 3.5.3 (2019-03-11) 1 NOTE (large subdirs)

## R CMD check results

There were no ERRORs or WARNINGs.  (There were warnings from the Fortran
compiler, owing to the existence of comments beyond the 72nd column in
src/magdec.f, which derives from code provided by IAGA, the Int'l Asoc. of
Geomagnetism and Aeronomy and that is kept in the original form for easier
tracing of its provenance.)

There was 1 NOTE, as follows.

* checking installed package size ... NOTE
  installed size is  9.4Mb
  sub-directories of 1Mb or more:
    doc    2.5Mb
    help   2.6Mb
    R      2.3Mb



0 errors | 0 warnings | 1 note

## Reverse dependencies

> revdepcheck::revdep_check()
── INIT ────────────────────────────────────────────── Computing revdeps ──
── INSTALL ────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of oce
Installing DEV version of oce
── CHECK ──────────────────────────────────────────────────── 7 packages ──
✔ dendroTools 1.0.5                      ── E: 0     | W: 0     | N: 0     
✔ graticule 0.1.2                        ── E: 0     | W: 0     | N: 0     
✔ ocedata 0.1.5                          ── E: 0     | W: 0     | N: 0     
✔ seacarb 3.2.12                         ── E: 0     | W: 0     | N: 0     
✔ skyscapeR 0.2.2                        ── E: 0     | W: 0     | N: 1     
✔ soundecology 1.3.3                     ── E: 0     | W: 0     | N: 0     
✔ SWMPr 2.3.1                            ── E: 0     | W: 0     | N: 0     
OK: 7                                                                    
BROKEN: 0
Total time: 19 min
── REPORT ─────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Warning message:
call dbDisconnect() when finished working with a connection 

---


