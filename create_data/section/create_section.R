library(oce)
section <- read.oce("a03_hy1.csv")
str(section[[1, "station"]]@metadata)

