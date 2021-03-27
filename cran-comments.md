# Resubmission of 1.4-0 (rejected at pretest stage)

This resubmission addresses a pretest problems relating to (a) the tarball exceeding 5e6 bytes and (b) an unlinkable reference to a function in the marmap package.  I have trimmed the tarball and changed the link{} to code{}.

# Tests

## Local Tests

Local MacOS-11.1 R-4.0.4 CMD (BUILD, INSTALL, CHECK): no ERRORs or WARNINGs but
the usual note on the author, plus another NOTE on sub-directories of 1MB or
more: (R 3.0Mb, data 1.2Mb, doc 2.3Mb, help 2.8Mb).

## Github R-CMD-check Action Tests

R-CMD-check github action reports no problems on
* windows-latest (release)
* macOS-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel).


## Remote Windows Checks

None of
```R
devtools:::build_win_release()
devtools:::build_win_oldrelease()
devtools:::build_win_devel()
```
reports any errors.

# Reverse Dependency Checks

Using
```
# devtools::install_github("r-lib/revdepcheck")
revdepcheck::revdep_check(timeout=30*60,num_workers=4)
```
identified no errors.

