Thoughts on submission.

Maybe the size is just too large.  Below are some thoughts on that.

# 1.2.0 on CRAN
```
   sub-directories of 1Mb or more:
      R      2.4Mb
      doc    2.3Mb
      help   2.7Mb
      libs   4.1Mb
```


# 1.3.0 trial 1 submitted 2020-01-27

tarball size 5.2M
```
$ ls -lh oce_1.3-0.tar.gz
-rw-r--r--  1 kelley  staff   5.2M 27 Jan 19:14 oce_1.3-0.tar.gz
```

# 1.3.0 trial 2 submitted 2020-01-27

I didn't record the tarball size.

This has (CRAN build)

# 1.3.0 trial 3A (not submitted) with figures moved out of package

I can drop the tarball size to 4.5M (is this under a limit??) by moving those
figures elsewhere.
```
ls -lh oce_1.3-0.tar.gz
-rw-r--r--  1 kelley  staff   4.5M 27 Jan 19:20 oce_1.3-0.tar.gz
```

This has (local build)
```
N  checking installed package size ...
     installed size is 10.6Mb
     sub-directories of 1Mb or more:
       R      3.0Mb
       data   1.2Mb
       doc    2.3Mb
       help   3.0Mb
```
NOTE: I may be able to shrink the data to under 1Mb, getting it below the NOTE
threshold.


# 1.3.0 trial 3B (not submitted) with topoNS.rda removed and AMSR.rda trimmed

Since the main purpose is for testing the code, I don't worry too much about
going for a smaller area.  Doing that, I get tarball size
```
ls -lh ../oce_1.3-0.tar.gz
-rw-r--r--  1 kelley  staff   4.3M 27 Jan 20:17 ../oce_1.3-0.tar.gz
```
and the report during local checking is
```
     installed size is 10.4Mb
     sub-directories of 1Mb or more:
       R      3.0Mb
       data   1.0Mb
       doc    2.3Mb
       help   3.0Mb
```
so not quite below the limit.  `amsr.rda` is only 9.8Kb now, so further trimming doesn't
seem worthwhile.

The top 4 files now are
* topoWorld.rda 349KB
* coastlineWorld.rda 106 KB
* landsat.rda 100 KB
and the first two cannot really be trimmed easily.  That leaves landsat.rda;
I'll try halving its size.

# 1.3.0 trial 3C (not submitted) with landsat.rda trimmed



