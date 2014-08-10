library(oce)
data(ctd)
## alter metadata
ctd[["longitude"]] <- 1
stopifnot(all.equal.numeric(ctd[["longitude"]], 1, 1e-5))
ctd[["latitude"]] <- 2
stopifnot(all.equal.numeric(ctd[["latitude"]], 2, 1e-5))
## alter data
S <- ctd[["sal"]] # tests abbreviations also
ctd[["salinity"]] <- S + 1
stopifnot(all.equal.numeric(ctd[["salinity"]], S+1, 1e-5))
top <- subset(ctd, pressure < 5)
stopifnot(max(top[['pressure']]) < 5)

