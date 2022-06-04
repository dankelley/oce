# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

# https://github.com/dankelley/oce/issues/1962
test_that("as.ctd(argo,profile=1L) flattens flags", {
    data(argo)
    for (profile in 1:2) {
        ctd <- as.ctd(argo, profile=profile)
        for (name in names(argo@metadata$flags)) {
            flagname <- paste0(name, "Flag")
            expect_true(is.array(argo[[flagname]]))
            expect_true(is.vector(ctd[[flagname]]))
            expect_equal(argo[[flagname]][,profile], ctd[[flagname]])
        }
    }
})

# https://github.com/dankelley/oce/issues/1891
test_that("ctd[[\"?\"]] works (issue 1891)", {
    data(section)
    ctd <- section[["station", 10]]
    # Test listing
    available <- ctd[["?"]]
    expect_true("time" %in% available$metadataDerived)
    expect_true("SA" %in% available$dataDerived)
    # Test whether missing still works
    expect_true(is.null(ctd[["bark"]]))
    # Test whether a stored value still works
    expect_equal(ctd[["salinity"]], ctd@data$salinity)
    # Test whether a computed value still works
    expect_equal(ctd[["SA"]], swAbsoluteSalinity(ctd))
})

# https://github.com/dankelley/oce/issues/1898
test_that("as.ctd() handles location well (issue 1898)", {
    data(ctd)
    l <- list(salinity=ctd[["salinity"]],
        temperature=ctd[["temperature"]],
        pressure=ctd[["pressure"]])
    ctd1 <- as.ctd(l)
    expect_false("longitude" %in% names(ctd1@metadata))
    expect_false("latitude" %in% names(ctd1@metadata))
    ctd2 <- as.ctd(l, longitude=-60, latitude=45)
    expect_true("longitude" %in% names(ctd2@metadata))
    expect_true("latitude" %in% names(ctd2@metadata))
})

test_that("ctdRepair() works as expected", {
    data(ctd)
    expect_warning(ctd <- ctdRepair(ctd), "^changed metadata\\$recoveryTime")
    ctd@data$latitude <- ctd@metadata$latitude
    expect_warning(ctd <- ctdRepair(ctd), "^moving unit\\-length data")
    ctd@data$longitude <- ctd@metadata$longitude
    expect_warning(ctd <- ctdRepair(ctd), "^moving unit\\-length data")
})

test_that("plotTS() handles differently EOSs correctly", {
    data(ctd)
    options(oceEOS="unesco")
    expect_silent(plotTS(ctd))
    expect_silent(plotTS(ctd, eos="unesco"))
    expect_silent(plotTS(ctd, eos="gsw"))
    options(oceEOS="gsw")
    expect_silent(plotTS(ctd))
    expect_silent(plotTS(ctd, eos="unesco"))
    expect_silent(plotTS(ctd, eos="gsw"))
})

test_that("as.ctd() with specified arguments, including salinity", {
    data(ctd)
    ctd_ctd <- as.ctd(salinity=ctd[["salinity"]], temperature=ctd[["temperature"]], pressure=ctd[["pressure"]])
    expect_equal(ctd[["salinity"]], ctd_ctd[["salinity"]])
    expect_equal(ctd[["temperature"]], ctd_ctd[["temperature"]])
    expect_equal(ctd[["pressure"]], ctd_ctd[["pressure"]])
    expect_equal(ctd_ctd[["temperatureUnit"]], list(unit=expression(degree*C), scale="ITS-90"))
    expect_equal(ctd_ctd[["pressureType"]], "sea")
    # check addition of a new column
    fluo <- rep(1, length(ctd_ctd[["salinity"]]))
    ctd_ctd <- oceSetData(ctd_ctd, name="fluorescence", value=fluo,
        unit=list(unit=expression(mg/m^3), scale=""))
    expect_equal(ctd_ctd[["fluorescenceUnit"]],
        list(unit=expression(mg/m^3), scale=""))
    expect_true("fluorescence" %in% names(ctd_ctd[["data"]]))
})

test_that("as.ctd() with a list of oce objects", {
    data(ctd)
    a1 <- ctd
    a2 <- subset(ctd, pressure < 10)
    a3 <- subset(ctd, pressure < 5)
    a <- vector("list", 3)
    a[[1]] <- a1
    a[[2]] <- a2
    a[[3]] <- a3
    A <- as.ctd(a)
    expect_equal(length(A[["salinity"]]),
        length(a1[["salinity"]]) + length(a2[["salinity"]]) + length(a3[["salinity"]]))
    expect_equal(length(A[["salinity"]]), length(A[["pressure"]]))
    expect_equal(length(A[["temperature"]]), length(A[["pressure"]]))
    expect_equal(length(A[["latitude"]]), length(A[["pressure"]]))
    expect_equal(length(A[["longitude"]]), length(A[["pressure"]]))
})

test_that("ctd[[\"CT\"]], ctd[[\"SA\"]] and ctd[[\"Sstar\"]] require location", {
    a <- as.ctd(35,10,0)
    expect_error(a[["CT"]], "need longitude and latitude to compute")
    expect_error(a[["SA"]], "need longitude and latitude to compute")
    expect_error(a[["Sstar"]], "need longitude and latitude to compute")
    b <- as.ctd(35,10,100,longitude=-40, latitude=30)
    expect_silent(b[["CT"]])
    expect_silent(b[["SA"]])
    expect_silent(b[["Sstar"]])
})

test_that("ctd[[\"CT\"]], ctd[[\"SA\"]] and ctd[[\"Sstar\"]] give expected values", {
    b <- as.ctd(35, 10, 1000, longitude=188, latitude=4)
    expect_silent(b[["CT"]])
    expect_equal(b[["SA"]], gsw::gsw_SA_from_SP(SP=35,p=1000,longitude=188,latitude=4))
    expect_equal(b[["Sstar"]], gsw::gsw_Sstar_from_SP(SP=35,p=1000,longitude=188,latitude=4))
    SA <- b[["SA"]]
    expect_equal(b[["CT"]], gsw::gsw_CT_from_t(SA, 10, 1000))
})

test_that("as.ctd() with specified arguments, not including salinity", {
    data(ctd)
    salinity <- ctd[["salinity"]]
    temperature <- ctd[["temperature"]]
    pressure <- ctd[["pressure"]]
    conductivity <- swCSTp(salinity, temperature, pressure)
    options(oceEOS="unesco")
    ctdNew <- as.ctd(conductivity=conductivity, temperature=temperature, pressure=pressure)
    # Test that all fields were created accurately.
    expect_equal(salinity, ctdNew[["salinity"]])
    expect_equal(temperature, ctdNew[["temperature"]])
    expect_equal(pressure, ctdNew[["pressure"]])
    ##
    options(oceEOS="gsw")
    ctdNew <- as.ctd(conductivity=conductivity, temperature=temperature, pressure=pressure)
    # Test that all fields were created accurately.
    expect_equal(salinity, ctdNew[["salinity"]])
    expect_equal(temperature, ctdNew[["temperature"]])
    expect_equal(pressure, ctdNew[["pressure"]])
})

test_that("as.ctd() with a data frame", {
    data(ctd)
    ctd_df <- as.ctd(data.frame(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
    expect_equal(ctd[["salinity"]], ctd_df[["salinity"]])
    expect_equal(ctd[["temperature"]], ctd_df[["temperature"]])
    expect_equal(ctd[["pressure"]], ctd_df[["pressure"]])
})

test_that("as.ctd() with a list", {
    data(ctd)
    ctd_l <- as.ctd(list(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
    expect_equal(ctd[["salinity"]], ctd_l[["salinity"]])
    expect_equal(ctd[["temperature"]], ctd_l[["temperature"]])
    expect_equal(ctd[["pressure"]], ctd_l[["pressure"]])
})

test_that("ctdTrim indices argument", {
    data(ctdRaw)
    a <- ctdRaw
    # Insert a crazy flag, not for conventional use, but only
    # to trace whether subsetting works as intended.
    a[["salinityFlag"]] <- seq_along(a[["salinity"]])
    b <- ctdTrim(a, method="sbe")
    c <- ctdTrim(a, method="sbe", indices=TRUE)
    for (name in names(b[["data"]])) {
        # Must use oceGetData because [["time"]] grabs 'time' from
        # the metadata, which is a scalar.
        expect_equal(oceGetData(a, name)[c], oceGetData(b, name))
    }
    # Demonstrate that it works for flags
    for (name in names(a[["flags"]])) {
        expect_equal(a[[paste(name, "flag", sep="")]][c], b[[paste(name, "flag", sep="")]])
    }
})

test_that("ctd subsetting and trimming", {
    # NOTE: this is brittle to changes in data(ctd), but that's a good thing, because
    # changing the dataset should be done only when really necessary, e.g. the July 2015
    # transition to use ITS-90 based temperature (because IPTS-68 was not
    # yet handled by oce at that time), and the April 2016
    # transition back to IPTS-68 for this dataset, once oce could handle
    # both scales.
    #
    # 1. SBE trimming method
    p <- c(rep(4, 1000),
        seq(4, 0.5, length.out = 50),
        seq(0.5, 100, length.out=1000),
        rep(100, 100),
        seq(100, 0, length.out=1000))
    S <- 35-p/100
    T <- 10+(100-p)/50
    d <- as.ctd(S, T, p)
    plotScan(d)
    dt <- ctdTrim(d, method="sbe")
    dt2 <- ctdTrim(d)
    #  2. trim by scan
    scanRange <- range(ctd[['scan']])
    newScanRange <- c(scanRange[1] + 20, scanRange[2] - 20)
    ctdTrimmed <- ctdTrim(ctd, "scan", parameters=newScanRange)
    # below are the data at the top of this trim spot
    # 150    149.000      6.198      6.148    11.7372    30.8882  0.000e+00
    # 151    150.000      6.437      6.384    11.6331    30.9301  0.000e+00
    # 152    151.000      6.770      6.715    11.4273    30.8928  0.000e+00
    expect_equal(ctdTrimmed[["scan"]][1:3], c(150,151,152))
    expect_equal(ctdTrimmed[["salinity"]][1:3], c(30.8882,30.9301,30.8928))
    expect_equal(ctdTrimmed[["pressure"]][1:3], c(6.198,6.437,6.770))
    # The next two lines check on whether the data in the object match
    # exactly the data file, and then whether the accessor converts from
    # the old temperature scale that is in the data file to the new one
    # that formulas are based on. There are changes here to account for
    # a change to data(ctd) that addressed a bug discussed in issue 1293.
    expect_equal(ctdTrimmed@data$temperature[1:3], c(11.7372,11.6331,11.4273))
    expect_equal(ctdTrimmed[["temperature"]][1:3], T90fromT68(c(11.7372,11.6331,11.4273)))
    # next is from a test for issue 669
    n <- length(ctd[["salinity"]])
    set.seed(669)
    lon <- ctd[["longitude"]] + rnorm(n, sd=0.05)
    lat <- ctd[["latitude"]] + rnorm(n, sd=0.05)
    ctdnew <- ctd
    # change from one-location object to multi-location one
    ctdnew@metadata$longitude <- NULL
    ctdnew@data$longitude <- lon
    ctdnew@metadata$latitude <- NULL
    ctdnew@data$latitude <- lat
    ctdnewSubset <- subset(ctdnew, 200 <= scan & scan <= 300)
    ctdnewTrim <- ctdTrim(ctdnew, method='scan', parameters = c(200, 300))
    expect_equal(ctdnewSubset[['salinity']], ctdnewTrim[['salinity']])
    expect_equal(ctdnewSubset[['scan']], ctdnewTrim[['scan']])
    expect_equal(length(ctdnewSubset[['scan']]), length(ctdnewSubset[['longitude']]))
})

test_that("ctd subsetting by index", {
    n <- 3                       # number of data to retain
    ctdTrimmed <- ctdTrim(ctd, "index", parameters=c(1, n))
    expect_equal(length(ctdTrimmed[["salinity"]]), n)
})


test_that("alter ctd metadata", {
    ctd[["longitude"]] <- 1
    expect_equal(ctd[["longitude"]], 1)
    ctd[["latitude"]] <- 2
    expect_equal(ctd[["latitude"]], 2)
    # alter data
    S <- ctd[["sal"]] # tests abbreviations also
    ctd[["salinity"]] <- S + 1
    expect_equal(ctd[["salinity"]], S+1)
    top <- subset(ctd, pressure < 5)
    expect_true(max(top[['pressure']]) < 5)
})

test_that("gsw calculations on ctd data", {
    SP <- 35
    t <- 10
    p <- 1000
    lon <- 300
    lat <- 30
    nctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
    expect_equal(nctd[["SP"]], SP)
    expect_equal(nctd[["t"]], t)
    expect_equal(nctd[["p"]], p)
    expect_equal(nctd[["SP"]], nctd[["salinity"]])
    expect_equal(nctd[["t"]], nctd[["temperature"]])
    expect_equal(nctd[["p"]], nctd[["pressure"]])
    Sstar <- gsw::gsw_Sstar_from_SP(SP, p=p, longitude=lon, latitude=lat)
    expect_equal(Sstar, nctd[["Sstar"]])
    SR <- gsw::gsw_SR_from_SP(SP=nctd[["SP"]])
    expect_equal(SR, nctd[["SR"]])
    expect_equal(SR, swSR(nctd))
    SA <- gsw::gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
    expect_equal(SA, nctd[["SA"]])
    Sstar <- gsw::gsw_Sstar_from_SA(SA=SA, p=p, longitude=lon, latitude=lat)
    expect_equal(Sstar, nctd[["Sstar"]])
})

test_that("accessors work as functions and [[", {
    expect_equal(swSigmaTheta(ctd), ctd[["sigmaTheta"]])
})

test_that("[[ works on SA, CT and spelled-out variants", {
    SA1 <- ctd[["SA"]]
    SA2 <- ctd[["Absolute Salinity"]]
    expect_equal(SA1, SA2)
    CT1 <- ctd[["CT"]]
    CT2 <- ctd[["Conservative Temperature"]]
    expect_equal(CT1, CT2)
})

test_that("ability to change conductivityUnit", {
    # These came from issue 731
    ctd2 <- ctd
    ctd2@data$conductivity <- swCSTp(ctd2) * 42.914
    ctd2[['conductivityUnit']] <- list(unit=expression(mS/cm), scale="")
    expect_equal(swSCTp(ctd2), ctd2[['salinity']], tolerance=1e-8) # OK on 64-bit OSX
    ctd3 <- ctd
    ctd3@data$conductivity <- swCSTp(ctd3) * 4.2914
    ctd3[['conductivityUnit']] <- list(unit=expression(S/m), scale="")
    expect_equal(swSCTp(ctd3), ctd3[['salinity']], tolerance=1e-8) # OK on 64-bit OSX
})

test_that("column renaming with a cnv file", {
    expect_warning(
        expect_warning(
            d1 <- read.oce(system.file("extdata", "ctd.cnv", package="oce")),
            "this CNV file has temperature in the IPTS\\-68 scale"),
        "startTime < 1950, suggesting y2k problem in this cnv file")
    expect_equal(names(d1[["data"]]),
        c("scan","timeS","pressure","depth","temperature","salinity","flag"))
    expect_warning(
        expect_warning(
            expect_warning(
                d2 <- read.oce(system.file("extdata", "ctd.cnv", package="oce"),
                    columns=list(FAKE=list(name="sal00",
                            unit=list(unit=expression(), scale="PSS-78")))),
                "this CNV file has temperature in the IPTS\\-68 scale"),
            "cannot find salinity or conductivity in .cnv file"),
        "startTime < 1950, suggesting y2k problem in this cnv file")
    expect_equal(names(d2[["data"]]),
        c("scan","timeS","pressure","depth","temperature","FAKE","flag"))
})


## A Dalhousie-produced cnv file.
##
##'** Ship:      Divcom3'
##'** Cruise:    Halifax Harbour'
##'** Station:   Stn 2'
##'** Latitude:  N44 41.056'
##'** Longitude: w63 38.633'
test_that("Dalhousie-produced cnv file", {
    expect_warning(
        expect_warning(
            d1 <- read.oce(system.file("extdata", "ctd.cnv", package="oce")),
            "this CNV file has temperature in the IPTS\\-68 scale"),
        "startTime < 1950, suggesting y2k problem in this cnv file")
    expect_equal(d1[["temperatureUnit"]]$unit, expression(degree*C))
    # NB. the file holds IPTS-68 but we # store ITS-90 internally
    expect_equal(d1[["temperatureUnit"]]$scale, "IPTS-68")
    expect_null(d1[["conductivityUnit"]]) # this file does not have conductivity
    expect_equal(d1[["pressureUnit"]]$unit, expression(dbar))
    expect_equal(d1[["pressureType"]], "sea")
    expect_equal(d1[["ship"]], "Divcom3")
    expect_equal(d1[["cruise"]], "Halifax Harbour")
    expect_equal(d1[["station"]], "Stn 2")
    expect_equal(d1[["latitude"]], 44+41.056/60)
    expect_equal(d1[["longitude"]], -(63+38.633/60))
    expect_equal(d1[['salinity']][1:3], c(29.9210, 29.9205, 29.9206))
    expect_equal(d1[['pressure']][1:3], c(1.480, 1.671, 2.052))
    # Check on IPTS-68 vs ITS-90 issue[s]
    expect_equal(d1@data$temperature[1:3], c(14.2245, 14.2299, 14.2285))
    expect_equal(d1[['temperature']][1:3], c(14.22108694, 14.22648564, 14.22508598))
    # Check that what we read here matches exactly data(ctd), as an
    # ensurance that any code changes to read.ctd.sbe() are always
    # followed by updates to data(ctd) via "make clean ; make ; make
    # install" carried out by the Makefile in the create_data/ctd directory.
    expect_equal(d1@data$temperature, ctd@data$temperature)
    expect_equal(d1[["temperature"]], ctd[["temperature"]])

})

## A file containing CTD data acquired in the Beaufort Sea in 2003.
## I am not sure if this was a standardized format, but I had to work
## with these data so I added support for it.  The files end in .ctd,
## but oceMagic() recognizes them from the first line.  Note the trailing
## space in the sample data:
##
##'SHIP = CCGS Louis S St.Laurent '
##'CASTNO = 1 '
##'DATE = 11-Aug-2003 '
##'LATITUDE (N)= 71.391 '
##'LONGITUDE (W)= 134.001 '
test_that("Beaufort sea data I (reading ctd/woce/exchange)", {
    d2 <- read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
    expect_equal(d2[["temperatureUnit"]], list(unit=expression(degree*C), scale="ITS-90"))
    expect_equal(d2[["pressureUnit"]], list(unit=expression(dbar), scale=""))
    expect_equal(d2[["pressureType"]], "sea")
    expect_equal(d2[["ship"]], "CCGS Louis S St.Laurent")
    expect_equal(d2[["station"]], "1")
    expect_equal(d2[["date"]], as.POSIXct("2003-08-11", tz="UTC"))
    expect_equal(d2[["latitude"]], 71.391)
    expect_equal(d2[["longitude"]], -134.001)
    expect_equal(d2[['pressure']][1:3], 1:3)
    expect_equal(d2[['temperature']][1:3], c(-1.1999, -1.2250, -1.2270))
    expect_equal(d2[['salinity']][1:3], c(28.4279, 27.9823, 28.0095))
})

## A file containing CTD data acquired in the Beaufort Sea in 20l2,
## in standard .cnv format (albeit with a date format that was
## not decoded until I added a new format to decodeTime(). Note
## also that the location is stated differently than in the above
## example.
##
##'** Ship:  CCGS Louis St-Laurent'
##'** Station:   BL1'
##'** Depth (m):  87'
##'* NMEA Latitude = 71 20.70 N'
##'* NMEA Longitude = 151 47.26 W'
##'* NMEA UTC (Time) = Aug 09 2012 06:34:34'
test_that("Beaufort sea data II", {
    d3 <- read.oce(system.file("extdata", "d201211_0011.cnv", package="oce"))
    expect_equal(d3[["temperatureUnit"]]$unit, expression(degree*C))
    expect_equal(d3[["temperatureUnit"]]$scale, "ITS-90")
    expect_equal(d3[["conductivityUnit"]]$unit, expression(mS/cm))
    expect_equal(d3[["pressureType"]], "sea")
    expect_equal(d3[["ship"]], "CCGS Louis St-Laurent")
    expect_equal(d3[["station"]], "BL1")
    expect_equal(d3[["date"]], as.POSIXct("2012-08-09 06:34:34", tz="UTC"))
    expect_equal(d3[["waterDepth"]], 87)
    expect_equal(d3[["latitude"]], 71+20.70/60)
    expect_equal(d3[["longitude"]], -(151+47.26/60))
    expect_equal(d3[['pressure']][1:3], c(1,2,3))
    expect_equal(d3[['temperature']][1:3], c(-0.0155,0.0005,0.0092))
    expect_equal(d3[['salinity']][1:3], c(25.1637,25.1964,25.3011))
})

test_that("pressure accessor handles psi unit", {
    porig <- ctd@data$pressure
    # fake data in psi ... [[]] should return in dbar
    ctd@data$pressure <- porig / 0.6894757 # 1 psi=6894.757Pa=0.6894756 dbar
    ctd@metadata$units$pressure <- list(unit=expression(psi), scale="")
    expect_warning(ptest <- ctd[["pressure"]], "converting pressure from PSI to dbar")
    expect_equal(porig, ptest)
})

test_that("pressure accessor handles missing pressure", {
    depth <- swDepth(ctd@data$pressure, latitude=ctd[["latitude"]], eos="unesco")
    porig <- ctd@data$pressure
    # remove existing
    ctd@data$pressure <- NULL
    ctd@metadata$units$pressure <- NULL
    # add new
    ctd2 <- oceSetData(ctd, name="depth", value=depth, unit=list(unit=expression(m), scale=""))
    # test
    expect_equal(porig, ctd2[['pressure']], tolerance=0.0001) # swDepth is approximate; sub-mm is good enough anyway
})

test_that("salinity accessor computes value from conductivity", {
    C <- swCSTp(ctd@data$salinity, ctd@data$temperature, ctd@data$pressure)
    Sorig <- ctd@data$salinity
    # remove existing
    ctd@data$salinity <- NULL
    ctd@metadata$units$salinity <- NULL
    # add new
    ctd2 <- oceSetData(ctd, name="conductivity", value=C, unit=list(unit=expression(), scale="PSS-78"))
    expect_warning(S <- ctd2[["salinity"]],
        "constructed salinity from temperature, conductivity-ratio and pressure")
    expect_equal(Sorig, S, tolerance=0.0001)
})

test_that("nitrate can be inferred from nitrite and NO2+NO3", {
    data(section)
    stn1 <- section[["station", 1]]
    expect_equal(stn1[["nitrate"]], stn1[["NO2+NO3"]] - stn1[["nitrite"]])
})

test_that("as.ctd(rsk) transfers information properly", {
    data(rsk)
    expect_equal(rsk@metadata$units$pressure$scale, "absolute")
    ctd <- as.ctd(rsk)
    expect_equal(ctd@metadata$units$pressure$scale, "sea")
    for (item in names(rsk@metadata)) {
        if (item != "units" && item != "flags" && item != "dataNamesOriginal") {
            expect_equal(rsk@metadata[[item]], ctd@metadata[[item]],
                label=paste("checking metadata$", item, sep=""),
                expected.label=rsk@metadata[[item]],
                info=paste("failed while checking metadata$", item, sep=""))
        }
    }
    for (item in names(rsk@data)) {
        if (item != "pressure") {
            expect_equal(rsk@data[[item]], ctd@data[[item]],
                label=paste("checking data$", item, sep=""),
                expected.label=rsk@data[[item]],
                info=paste("failed while checking data$", item, sep=""))
        }
    }
    expect_equal(ctd[['pressure']], rsk[['pressure']] - rsk[['pressureAtmospheric']])
    ctd <- as.ctd(rsk, pressureAtmospheric=1)
    expect_equal(ctd[['pressure']], rsk[['pressure']] - rsk[['pressureAtmospheric']] - 1)
    # specify some values to check that we can over-ride some metadata
    latitude <- 42.244
    longitude <- -8.76
    ctd <- as.ctd(rsk,
        latitude=latitude,
        longitude=longitude,
        ship="SHIP",
        cruise="CRUISE",
        station="STATION",
        deploymentType="DEPLOYMENTTYPE")
    expect_equal(ctd[["latitude"]], latitude)
    expect_equal(ctd[["longitude"]], longitude)
    expect_equal(ctd[["cruise"]], "CRUISE")
    expect_equal(ctd[["ship"]], "SHIP")
    expect_equal(ctd[["station"]], "STATION")
    expect_equal(ctd[["deploymentType"]], "DEPLOYMENTTYPE")
})

test_that("ctdFindProfiles", {
    S <- ctd[["salinity"]]
    T <- ctd[["temperature"]]
    p <- ctd[["pressure"]]
    n <- 10                      # number of fake profiles
    SS <- rep(c(S, rev(S)), n)
    TT <- rep(c(T, rev(T)), n)
    pp <- rep(c(p, rev(p)), n)
    towyow <- as.ctd(SS, TT, pp, latitude=ctd[["latitude"]], longitude=ctd[["longitude"]])
    casts <- ctdFindProfiles(towyow)
    expect_equal(length(casts), n)
})

test_that("original names pair with final names", {
    # This should help to ensure that bug 1141 does not return
    f <- system.file("extdata", "d201211_0011.cnv", package="oce")
    d <- read.oce(f)
    expect_equal(names(d[["data"]]),
        c("scan", "pressure", "depth", "temperature",
            "temperature2", "conductivity", "conductivity2",
            "oxygenRaw", "beamTransmission", "v1", "fluorescence",
            "v0", "fluorescence2", "v4", "upoly", "par", "spar",
            "altimeter", "oxygen", "salinity", "salinity2",
            "theta", "sigmaTheta", "soundSpeed", "nbin", "flag"))
    dno <- d[["dataNamesOriginal"]]
    expect_equal(dno$scan, "scan")
    expect_equal(dno$pressure, "prDM")
    expect_equal(dno$depth, "depSM")
    expect_equal(dno$temperature, "t090C")
    expect_equal(dno$temperature2, "t190C")
    expect_equal(dno$conductivity, "c0mS/cm")
    expect_equal(dno$conductivity2, "c1mS/cm")
    expect_equal(dno$oxygenRaw, "sbeox0V")
    expect_equal(dno$beamTransmission, "CStarTr0")
    expect_equal(dno$v1, "v1")
    expect_equal(dno$fluorescence, "flSP")
    expect_equal(dno$v0, "v0")
    expect_equal(dno$fluorescence2, "wetCDOM")
    expect_equal(dno$v4, "v4")
    expect_equal(dno$upoly, "upoly0")
    expect_equal(dno$par, "par")
    expect_equal(dno$spar, "spar")
    expect_equal(dno$altimeter, "altM")
    expect_equal(dno$oxygen, "sbeox0ML/L")
    expect_equal(dno$salinity, "sal00")
    expect_equal(dno$salinity2, "sal11")
    expect_equal(dno$theta, "potemp090C")
    # nexttest is commented out because it may not work on windows
    expect_equal(dno$sigmaTheta, "sigma-\xe900")
    expect_equal(dno$soundSpeed, "svCM")
    expect_equal(dno$nbin, "nbin")
    expect_equal(dno$flag, "flag")
})

test_that("setting and handling flags", {
    ctdQC <- initializeFlags(ctd, name="salinity", 2)
    ctdQC <- setFlags(ctdQC, name="salinity", i=1:10, value=3)
    expect_equal(ctdQC[["salinityFlag"]], c(rep(3, 10), rep(2, length(ctdQC[["salinity"]])-10)))
    ctdCleaned <- handleFlags(ctdQC, flags=c(1, 3:9))
    expect_true(all(is.na(ctdCleaned[["salinity"]][1:10])))
    expect_equal(tail(ctdCleaned[["salinity"]], -10), tail(ctd[["salinity"]], -10))
})

test_that("conductivity unit", {
    # Need to add conductivity; may as well shorten for speed.
    ctd <- subset(ctd, pressure < 10)
    C <- swCSTp(ctd)
    # Try all possible combinations
    a <- oceSetData(ctd, "conductivity", C, list(unit=expression(), scale=""))
    expect_equal(C, a[["conductivity", "uS/cm"]] / 42914)
    expect_equal(C, a[["conductivity", "mS/cm"]] / 42.914)
    expect_equal(C, a[["conductivity", "S/m"]] / 4.2914)
    expect_equal(C, a[["conductivity", "ratio"]])
    expect_equal(C, a[["conductivity", ""]])
    a <- oceSetData(ctd, "conductivity", C*42914, list(unit=expression(uS/cm), scale=""))
    expect_equal(C, a[["conductivity", "uS/cm"]] / 42914)
    expect_equal(C, a[["conductivity", "mS/cm"]] / 42.914)
    expect_equal(C, a[["conductivity", "S/m"]] / 4.2914)
    expect_equal(C, a[["conductivity", "ratio"]])
    expect_equal(C, a[["conductivity", ""]])
    a <- oceSetData(ctd, "conductivity", C*42.914, list(unit=expression(mS/cm), scale=""))
    expect_equal(C, a[["conductivity", "uS/cm"]] / 42914)
    expect_equal(C, a[["conductivity", "mS/cm"]] / 42.914)
    expect_equal(C, a[["conductivity", "S/m"]] / 4.2914)
    expect_equal(C, a[["conductivity", "ratio"]])
    expect_equal(C, a[["conductivity", ""]])
    a <- oceSetData(ctd, "conductivity", C*4.2914, list(unit=expression(S/m), scale=""))
    expect_equal(C, a[["conductivity", "uS/cm"]] / 42914)
    expect_equal(C, a[["conductivity", "mS/cm"]] / 42.914)
    expect_equal(C, a[["conductivity", "S/m"]] / 4.2914)
    expect_equal(C, a[["conductivity", "ratio"]])
    expect_equal(C, a[["conductivity", ""]])
})

test_that("as.ctd() handles multiple longitude and latitude", {
    # test code for issue 1440 (https://github.com/dankelley/oce/issues/1440)
    for (n in c(1, 20)) {
        T <- seq(2,3, length.out=n)
        S <- seq(31.4, 31.6, length.out=n)
        p <- seq(0, n, length.out=n)
        lat <- rep(44.5, n)
        lon <- rep(-63, n)
        stn <- 'fake'
        time <- seq(from=as.POSIXct("2012-01-01 00:00", tz='UTC'), by='min', length.out=20)
        ctd <- as.ctd(salinity=S,
            temperature=T,
            pressure=p,
            time=time,
            longitude=lon,
            latitude=lat,
            station=stn,
            startTime=min(time))
        if (n > 1) {
            expect_true("longitude" %in% names(ctd[["data"]]))
            expect_false("longitude" %in% names(ctd[["metadata"]]))
            expect_equal(length(ctd[["data"]]$longitude), n)
            expect_equal(length(ctd[["longitude"]]), n)
            expect_true("latitude" %in% names(ctd[["data"]]))
            expect_false("latitude" %in% names(ctd[["metadata"]]))
            expect_equal(length(ctd[["data"]]$latitude), n)
            expect_equal(length(ctd[["latitude"]]), n)
        } else {
            expect_false("longitude" %in% names(ctd[["data"]]))
            expect_true("longitude" %in% names(ctd[["metadata"]]))
            expect_false("latitude" %in% names(ctd[["data"]]))
            expect_true("latitude" %in% names(ctd[["metadata"]]))
            expect_equal(length(ctd[["longitude"]]), 1)
            expect_equal(length(ctd[["metadata"]]$longitude), 1)
            expect_equal(length(ctd[["latitude"]]), 1)
            expect_equal(length(ctd[["metadata"]]$latitude), 1)
        }
    }
})

test_that("lon360() works", {
    ctdShifted <- lon360(ctd)
    expect_equal(360 + ctd[["longitude"]], ctdShifted[["longitude"]])
})

# Below an excerpt from the docs, as of 2022-03-01
# 1 "salinity+temperature"
# 2 "density+N2"
# 3 "TS"
# 4 "text"
# 5 "map"
# 5.1 as for which=5, except that the file name is drawn above the map.
# 6 "density+dpdt"
# 7 "density+time"
# 8 "index"
# 9 "salinity"
# 10 "temperature"
# 11 "density"
# 12 "N2"
# 13 "spice"
# 14 "tritium"
# 15 "Rrho"
# 16 "RrhoSF"
# 17 "conductivity"
# 20 "CT"
# 21 "SA"
# 30 "Sts"
# 31 "Tts"
# 32 "pts"
# 33 "rhots"

test_that("erroneous numeric 'which' is handled", {
    expect_error(plot(ctd, which=999), "which=\"999\" cannot be handled")
})

test_that("erroneous textual 'which' is handled", {
    expect_error(plot(ctd, which="unhandled"), "which=\"unhandled\" cannot be handled")
})

test_that("numeric 'which' works", {
    for (w in c(5.1, 1:13, 15:17, 20:21, 30:33)) {
        expect_silent(plot(ctd, which=w))
    }
})

test_that("character 'which' works", {
    for (w in c("salinity+temperature", "density+N2", "TS", "text", "map",
            "density+dpdt", "density+time", "index", "salinity", "temperature",
            "density", "N2", "spice", "Rrho", "RrhoSF",
            "conductivity", "CT", "SA", "Sts", "Tts", "pts", "rhots")) {
        expect_silent(plot(ctd, which=w))
    }
})


