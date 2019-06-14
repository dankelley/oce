## Test environments

* local R-3.6.0 MacOS install OK
* win-builder R-release
* win-builder R-devel
* win-builder R-oldrelease

## R CMD check results

There were no ERRORs or WARNINGs.  (There were many warnings from the Fortran
compiler, owing to the existence of comments beyond the 72nd column in
src/magdec.f (which is code derived from IAGA, the Int'l Asoc. of Geomagnetism
and aeronomy.) Although deleting the comments would be easy, doing so would
make the code more difficult to understand, and remove its similarity to the
original IAGA source. Therefore, those compiler messages were ignored. However,
src/magdec.f *does* differ in two ways from the original IAGA code, both
relating to making a specific type conversion, instead of relying on the
Fortran rules on automatic integer/real type conversion.

There was 1 NOTE, as follows.

* checking installed package size ... NOTE
  installed size is  9.4Mb
  sub-directories of 1Mb or more:
    doc    2.5Mb
    help   2.6Mb
    R      2.3Mb



0 errors | 0 warnings | 1 note

## Reverse dependencies

devtools::revdep_check() reports as follows

✔ seacarb 3.2.12                         ── E: 0     | W: 0     | N: 0                                    
✔ ocedata 0.1.5                          ── E: 0     | W: 0     | N: 0                                    
✔ graticule 0.1.2                        ── E: 0     | W: 0     | N: 0                                    
✔ skyscapeR 0.2.2                        ── E: 0     | W: 0     | N: 1                                    
✔ dendroTools 1.0.5                      ── E: 0     | W: 0     | N: 0                                    
✔ soundecology 1.3.3                     ── E: 0     | W: 0     | N: 0                                    
✔ SWMPr 2.3.1                            ── E: 0     | W: 0     | N: 0                                    
OK: 7                                                                                                   
BROKEN: 0

---


