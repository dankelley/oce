library(oce)
load("coastlineWorld.rda")
if (!interactive()) png("check_coastlineWorld.png")
plot(coastlineWorld)
if (!interactive()) dev.off()

