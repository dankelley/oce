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

## GSW tests
SP <- 35
t <- 10
p <- 1000
lon <- 300
lat <- 30
ctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
stopifnot(all.equal.numeric(ctd[["SP"]], SP))
stopifnot(all.equal.numeric(ctd[["t"]], t))
stopifnot(all.equal.numeric(ctd[["p"]], p))
stopifnot(all.equal.numeric(ctd[["SP"]], ctd[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["t"]], ctd[["temperature"]]))
stopifnot(all.equal.numeric(ctd[["p"]], ctd[["pressure"]]))
Sstar <- gsw_Sstar_from_SP(SP, p=p, longitude=lon, latitude=lat)
stopifnot(all.equal.numeric(Sstar, ctd[["Sstar"]]))
SR <- gsw_SR_from_SP(SP=ctd[["SP"]])
stopifnot(all.equal.numeric(SR, ctd[["SR"]]))
SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
stopifnot(all.equal.numeric(SA, ctd[["SA"]]))
Sstar <- gsw_Sstar_from_SA(SA=SA, p=p, longitude=lon, latitude=lat)
stopifnot(all.equal.numeric(Sstar, ctd[["Sstar"]]))

