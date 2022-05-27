A "?" in the first column means I've tried to shorten the timing, but don't yet
have timing results.  A 1 there means it's the first value, a 2 is a second row
with an update, etc.

The total time is 43.9 s, and I'll try for something in the range of 30 s,
mostly as an exercise.  (Often, trimming examples makes documentation clearer,
anyway.)

                           name user system elapsed percent cumpercent
1           plot-section-method 3.32   0.17    3.50     7.6        7.6
?                   sectionGrid 2.91   0.18    3.11     6.7       14.3
1                      addSpine 2.75   0.19    2.93     6.3       20.6
1               plot-ctd-method 1.80   0.17    1.97     4.1       24.8
1              plot-argo-method 1.75   0.06    1.82     4.0       28.8
1                        plotTS 1.58   0.16    1.73     3.6       32.4
1                 section-class 1.54   0.20    1.75     3.5       36.0
1            subset-argo-method 1.45   0.21    1.65     3.3       39.3
1             subset-ctd-method 1.42   0.06    1.49     3.3       42.6
1              subset-cm-method 1.34   0.02    1.36     3.1       45.7
1                plot-cm-method 1.28   0.03    1.31     2.9       48.6
?CTD_BCD2014666_008_1_DN.ODF.gz 1.11   0.03    1.25     2.6       51.2
1                   ctd_aml.csv 1.10   0.02    1.11     2.5       53.7
1               beamUnspreadAdp 1.00   0.00    1.00     2.3       56.0
1                       despike 0.99   0.01    1.00     2.3       58.3
1                      argoGrid 0.97   0.39    1.36     2.2       60.5
1                        imagep 0.97   0.02    0.99     2.2       62.7
1                 sectionSmooth 0.94   0.14    1.07     2.2       64.9
1       plot-echosounder-method 0.87   0.00    0.88     2.0       66.9
1                    lonlat2map 0.75   0.05    0.80     1.7       68.6
1    handleFlags-section-method 0.64   0.12    0.76     1.5       70.1
1                           rsk 0.58   0.05    0.63     1.3       71.4
1                      read.oce 0.55   0.00    0.55     1.3       72.7
1                      read.odf 0.53   0.00    0.54     1.2       73.9
1         initialize-ctd-method 0.51   0.01    0.54     1.2       75.1
