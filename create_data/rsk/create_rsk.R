rm(list=ls())
library(oce)
raw <- read.oce('060130_20150904_1159.rsk')
focus <- structure(c(1441381020.7302, 1441381482.90486), class = c("POSIXct", 
"POSIXt"), tzone = "UTC")
rsk <- subset(raw, focus[1] <= time & time <= focus[2])
save(file='rsk.rda', rsk)
tools::resaveRdaFiles("rsk.rda")
