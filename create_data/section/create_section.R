library(oce)
section <- read.section("a03_hy1.csv", sectionId="a03", institute="SIO",
    ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov")
save(section, file="section.rda")
tools::resaveRdaFiles("section.rda")
