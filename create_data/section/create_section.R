library(oce)
# next worked 2024-10-12
# download.file("https://cchdo.ucsd.edu/data/7872/a03_hy1.csv", "a03_hy1.csv")
section <- read.section("a03_hy1.csv",
    sectionId = "a03", institute = "SIO",
    ship = "R/V Professor Multanovskiy", scientist = "Vladimir Tereschenkov"
)
section <- initializeFlagScheme(section, "WHP bottle")
summary(section)

# Save in version 2, because otherwise users with R 3.5.x and earlier will not
# be able to use data("section"). My tests on 2024-10-12 suggest that
# the difference in filesize is under 1 percent.
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), "3.6.0") >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(section, file = "section.rda", version = 2)
    tools::resaveRdaFiles("section.rda", version = 2)
} else {
    save(section, file = "section.rda")
    tools::resaveRdaFiles("section.rda")
}
