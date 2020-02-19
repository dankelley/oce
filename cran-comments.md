## Test environments

* local MacOS R-3.6.2 CMD (BUILD, INSTALL, CHECK): no ERRORs or WARNINGs, but 1
  NOTE on sub-directories of 1MB or more: (R 2.4Mb, doc 2.3Mb, help 2.7Mb).
* rhub checks failed, with problems on the build machine (out of file space,
  etc)
* win-builder on R-devel?
* win-builder on R-release: OK.
* win-builder on R-old-release: 1 NOTE on file size (as at first bullet,
  above).


## revdepcheck::revdep_check() results

* no failures
* no problems

### Summary

* > revdep_summary()
* ✓ dendroTools 1.0.7                      ── E: 0     | W: 0     | N: 0    
* ✓ graticule 0.1.2                        ── E: 0     | W: 0     | N: 0    
* ✓ morphomap 1.1                          ── E: 0     | W: 0     | N: 0    
* ✓ oceanwaves 0.1.0                       ── E: 0     | W: 0     | N: 0    
* ✓ seacarb 3.2.12                         ── E: 0     | W: 0     | N: 0    
* ✓ skyscapeR 0.2.2                        ── E: 0     | W: 0     | N: 1    
* ✓ soundecology 1.3.3                     ── E: 0     | W: 0     | N: 0    
* ✓ SWMPr 2.3.1                            ── E: 0     | W: 0     | N: 0    

### Platform

|field    |value                        |
|:--------|:----------------------------|
|version  |R version 3.6.2 (2019-12-12) |
|os       |macOS Catalina 10.15.4       |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio                      |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|ctype    |en_CA.UTF-8                  |
|tz       |America/Halifax              |
|date     |2020-02-19                   |

### Dependencies

|package  |old   |new    |Δ  |
|:--------|:-----|:------|:--|
|oce      |1.1-1 |1.2-0  |*  |
|callr    |NA    |3.4.2  |*  |
|digest   |NA    |0.6.24 |*  |
|processx |NA    |3.4.2  |*  |
|ps       |NA    |1.3.2  |*  |

### Revdeps


