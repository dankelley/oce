# for a checklist see e.g. https://github.com/dankelley/oce/issues/2068
requireNamespace(c("codemetar", "devtools", "urlchecker", "rhub", "revdepcheck"))
# codemeta changes a timestamp, so requiring a commit after every call. That is
# senseless, so I only run the false part of the following conditional in the
# run-up to a release.
if (FALSE) {
    codemetar::write_codemeta()
} else {
   message("run 'codemetar::write_codemeta()' and then git push")
}
t <- devtools::spell_check()
stopifnot(t == "No spelling errors found.")
urlchecker::url_check()

# devtools checks.
# These are reliable, and useful, in contrast to the rhub checks that follow.
devtools::check_mac_release()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_win_oldrelease()

# Rhub checks.
# [2023-03-26] The next two checks are not very reliable.  Quite often,
# a run gets to the end with no problems but the system reports a PREPERROR.
# More rarely, but certainly not uncommonly, the test system dies
# before it gets to the stage of actually trying to build oce.  And,
# even when these tests (and all others) pass, sometimes the CRAN machines
# report other problems.  Given that the above block always seems
# to be useful, and that the next one is so unreliable, I don't know
# if there is any point in keeping the next.  Oh, and bonus: the
# tests in the next block often don't report for half a day.
if (FALSE) {
    rhub::check_for_cran(email="Dan.Kelley@Dal.Ca", show_status=FALSE)
    rhub::check(platform="debian-clang-devel", show_status=FALSE)
}
#> rhub::platforms()
#debian-clang-devel:
#    Debian Linux, R-devel, clang, ISO-8859-15 locale
#> rhub::check_rhub()

# Reverse dependency checks.
# remotes::install_github("r-lib/revdepcheck")
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers=4)
message("run following if desired: pkgdown::build_site()")
