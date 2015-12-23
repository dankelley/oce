library(oce)
try(source("~/src/oce/R/adv.R"))
try(source("~/src/oce/R/adv.sontek.R"))
try(source("~/src/oce/R/adv.nortek.R"))
seed <- read.table("../seed")
.Random.seed <- t(seed)
load("/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_enu.rda")
adv<-window(m05VectorEnu, as.POSIXct("2008-07-01 00:00:00", tz="UTC"), as.POSIXct("2008-07-01 00:01:00",tz="UTC"))
adv@metadata$filename <- "(file name redacted)"
adv@metadata$serialNumber <- "(serial number redacted)"
adv@metadata$headSerialNumber <- "(head serial number redacted)"
adv@metadata$latitude <- round(adv@metadata$latitude) 
adv@metadata$longitude <- round(adv@metadata$longitude) 
adv@metadata$deploymentName <- "(deployment name redacted)"
adv@metadata$comments <- "sample ADV file"
adv@processingLog$time <- adv@processingLog$time[1]
n <- length(adv@data$time)
adv@data$v[,1] <- adv@data$v[,1] + rnorm(n=n, sd=sd(0.05 * adv@data$v[,1]))
adv@data$v[,2] <- adv@data$v[,2] + rnorm(n=n, sd=sd(0.05 * adv@data$v[,2]))
adv@data$v[,3] <- adv@data$v[,3] + rnorm(n=n, sd=sd(0.05 * adv@data$v[,3]))
adv@processingLog$value <- "create sample file, based on perturbed data"
#save(adv, file="~/src/R-kelley/oce/data/adv.rda")
save(adv, file='adv.rda')
library(tools)
resaveRdaFiles("adv.rda", compress="auto")
str(adv)
