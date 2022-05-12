01.R: how much can we speed up the examples in the docs?

In the first 3 out files I was (stupidly) summing just the cases that took more
than 1s each.  That was not what I needed.  Starting with 04, I have total
time.  My impression is that the first 3 steps trimmed 1 to 2 sec each.

* 01_01.out: not sure what this is

* 01_02.out: trim plot,ctd-method and plot,section-method

* 01_03.out: trim subset,cm-method and magneticField

* 01_04.out: total 33.432 sec. trim plot,cm-method, subset,argo-method and section-class

* 01_05.out: total 28.856 sec. trim subset,ctd-method, oceCRS, argoGrid, plotTS

* 01_06.out: total 20.622 sec. trim mapPolygon, despike and ctd_aml.csv


