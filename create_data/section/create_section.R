library(oce)
source("~/src/oce/R/section.R")
section <- read.section("a03_hy1.csv", sectionId="a03", institute="SIO",
                        ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov") 
save(section, file="section.rda")
tools::resaveRdaFiles("section.rda")
n <- length(section[['station',1]]@data)
names <- names(section[['station',1]]@data)
par(mfrow=c(3,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # I checked: n==9
for (name in names) {
    hist(section[[name]], main="", xlab=name) # a check on issue 983
}
