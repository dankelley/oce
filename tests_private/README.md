This directory is used by the developers to do perform tests that are in
addition to those in the `tests` directory.  These additional tests rely on
private files that are not included in the CRAN version of oce, owing to
reasons of file size and privacy.

Some of the data files used here are stored in a Dropbox folder shared by
developers DK and CR, and others are stored in local files. The scheme is to
loop over the results of `list.files()`, and that means that the tests should
still work even files are missing.

In some cases, e.g. for adp/RDI files, the tests merely see whether data can be
read without error. In other cases, `summary` and `plot` are used, on the
expectation that the person running tests will scan the files visually to look
for problems (e.g. missing units for some SBE data types).

Files:

* README.md this file
* Makefile use this to run tests
* adp_rdi_01.R tests some adp datasets of the RDI variety
* ctd_beaufort_sea.R tests CTD data from Beaufort Sea (private data in DK possession)
* ctd_flemish_cap.R tests some CTD data from Flemish Cap (private data in DK possession)
* ctd_sleiwex_coriolissII.R tests some CTD data from SLEIWEX experiment (private data in DK and CR possession)
* odf_01.R tests various ODF datasets (private data in DK possession)

