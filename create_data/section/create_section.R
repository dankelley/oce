library(oce)
## next worked 2017-02-20
# download.file("https://cchdo.ucsd.edu/data/7872/a03_hy1.csv", "a03_hy1.csv")
section <- read.section("a03_hy1.csv", sectionId="a03", institute="SIO",
                        ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov") 
section <- initializeFlagScheme(section, "WHP bottle")
save(section, file="section.rda")
tools::resaveRdaFiles("section.rda")
n <- length(section[['station',1]]@data)
names <- names(section[['station',1]]@data)
par(mfrow=c(3,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # I checked: n==9
for (name in names) {
    hist(section[[name]], main="", xlab=name) # a check on issue 983
}
