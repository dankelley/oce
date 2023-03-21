#install.packages("codemetar")
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
print(t)
stopifnot(t == "No spelling errors found.")
urlchecker::url_check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_win_oldrelease()
# rhub broken (2022 Jun,Jul), working (2022 Aug and later)
rhub::check_for_cran(email="Dan.Kelley@Dal.Ca", show_status=FALSE)
rhub::check(platform="debian-clang-devel", show_status=FALSE)
#> rhub::platforms()
#debian-clang-devel:
#    Debian Linux, R-devel, clang, ISO-8859-15 locale
#> rhub::check_rhub()
# remotes::install_github("r-lib/revdepcheck")
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers=4)

# Use these in a GH comment to indicate progress towards a merge or commit.
# * [ ] local build + check
# * [ ] `devtools::spell_check()`
# * [ ] `urlchecker::url_check()`
# * [ ] `devtools::check_win_release()`
