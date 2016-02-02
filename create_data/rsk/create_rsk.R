rm(list=ls())
library(oce)
raw <- read.oce('060130_20150904_1159.rsk')
focus <- structure(c(1441381041, 1441381483), class = c("POSIXct", 
"POSIXt"), tzone = "UTC")
rsk <- subset(raw, focus[1] <= time & time <= focus[2])
save(file='rsk.rda', rsk)
tools::resaveRdaFiles("rsk.rda")
