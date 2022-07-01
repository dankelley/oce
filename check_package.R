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
devtools::check_win_release(email="Dan.Kelley@Dal.Ca")
devtools::check_win_devel(email="Dan.Kelley@Dal.Ca")
devtools::check_win_oldrelease(email="Dan.Kelley@Dal.Ca")
# rhub is broken as of June/July 2022
if (FALSE) {
    rhub::check_for_cran(email="Dan.Kelley@Dal.Ca")
}
# remotes::install_github("r-lib/revdepcheck")
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers=4)

