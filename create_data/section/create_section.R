library(oce)
## next worked 2017-02-20
## download.file("https://cchdo.ucsd.edu/data/7872/a03_hy1.csv", "a03_hy1.csv")
section <- read.section("a03_hy1.csv", sectionId="a03", institute="SIO",
                        ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov") 
section <- initializeFlagScheme(section, "WHP bottle")

if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    save(section, file="section.rda", version=2)
    tools::resaveRdaFiles('section.rda', version=2)
} else {
    save(section, file="section.rda")
    tools::resaveRdaFiles('section.rda')
}



