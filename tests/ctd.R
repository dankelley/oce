library(oce)
data(ctd)

## 1. as.ctd() with various first args
ctd_ctd <- as.ctd(salinity=ctd[["salinity"]], temperature=ctd[["temperature"]], pressure=ctd[["pressure"]])
stopifnot(all.equal.numeric(ctd[["salinity"]], ctd_ctd[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["temperature"]], ctd_ctd[["temperature"]]))
stopifnot(all.equal.numeric(ctd[["pressure"]], ctd_ctd[["pressure"]]))
ctd_df <- as.ctd(data.frame(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["salinity"]], ctd_df[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["temperature"]], ctd_df[["temperature"]]))
stopifnot(all.equal.numeric(ctd[["pressure"]], ctd_df[["pressure"]]))
ctd_l <- as.ctd(list(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["salinity"]], ctd_l[["salinity"]]))
stopifnot(all.equal.numeric(ctd[["temperature"]], ctd_l[["temperature"]]))
stopifnot(all.equal.numeric(ctd[["pressure"]], ctd_l[["pressure"]]))

## 2. alter metadata
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

## 3. GSW
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

