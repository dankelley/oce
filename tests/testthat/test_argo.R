# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(argo)

test_that("as.ctd works with built-in argo dataset", {
    expect_silent(as.ctd(argo[["profile", 1]]))
    expect_warning(as.ctd(argo[["profile", 1:5]]), "since 'profile' not given, defaulting to 1")
})

test_that("global attributes in metadata", {
    expect_equal(argo[["title"]], "Argo float vertical profile")
    expect_equal(argo[["institution"]], "")
    expect_equal(argo[["source"]], "Argo float")
    expect_equal(argo[["history"]], "2015-01-10T03:05:00Z creation")
    expect_equal(argo[["references"]], "http://www.argodatamgt.org/Documentation")
    expect_equal(argo[["userManualVersion"]], "3.03")
    expect_equal(argo[["conventions"]], "Argo-3.0 CF-1.6")
    expect_equal(argo[["featureType"]], "trajectoryProfile")
})

test_that("[[,argo-method", {
    options(oceEOS = "gsw")
    expect_equal(
        argo[["SA"]][1:2, 1:2],
        structure(c(
            35.3509423279029, 35.3529478543978, 35.3600216239489,
            35.3600133661509
        ), .Dim = c(2L, 2L))
    )
    expect_equal(
        argo[["CT"]][1:2, 1:2],
        structure(c(
            9.69604608349391, 9.69651156306521, 9.58902309316286,
            9.58644078639155
        ), .Dim = c(2L, 2L))
    )
    expect_equal(
        argo[["sigmaTheta"]][1:2, 1:2],
        structure(c(
            27.1479134860076, 27.1493888071818, 27.172918709517,
            27.173344472757
        ), .Dim = c(2L, 2L))
    )
    expect_equal(
        argo[["theta"]][1:2, 1:2],
        structure(c(
            9.70945684069746, 9.70995897509861, 9.60251640507005,
            9.59993195546977
        ), .Dim = c(2L, 2L))
    )
    # longitude/latitude expansion case 1: SA
    # https://github.com/dankelley/oce/issues/1911
    col <- 2
    SP <- argo[["salinity"]][, col]
    p <- argo[["pressure"]][, col]
    lon <- rep(argo[["longitude"]][col], length(SP))
    lat <- rep(argo[["latitude"]][col], length(SP))
    SA <- swAbsoluteSalinity(SP, p, lon, lat)
    expect_equal(argo[["SA"]][, col], SA)
    # Verify that if we use wrong longitude (say), the results change. This
    # helps to build confidence that we have decoded location correctly.
    lon1 <- rep(argo[["longitude"]][1 + col], length(SP))
    lat1 <- rep(argo[["latitude"]][1 + col], length(SP))
    SA1 <- swAbsoluteSalinity(SP, p, lon1, lat1)
    expect_false(identical(argo[["SA"]][, col], SA1))
    # longitude/latitude expansion case 1: Sstar
    Sstar <- swSstar(SP, p, lon, lat)
    expect_equal(argo[["Sstar"]][, col], Sstar)
    Sstar1 <- swSstar(SP, p, lon1, lat1)
    expect_false(identical(argo[["Sstar"]][, col], Sstar1))

    # test two ways of selecting by profile sequence number. We do not
    ## test for equality of the whole objects, because the processingLog
    ## slots are different.
    sub1 <- argo[["profile", 2:3]]
    sub2 <- subset(argo, profile %in% 2:3)

    expect_equal(sub1[["metadata"]], sub2[["metadata"]])
    expect_equal(sub1[["data"]], sub2[["data"]])
})

test_that("subset(argo, pressure < 500))", {
    data(argo)
    pcut <- 500
    top <- subset(argo, pressure < pcut)
    # test a few fields in a few profiles
    pressure <- argo[["pressure"]]
    for (i in 1:5) {
        pressure <- argo[["pressure"]][, i]
        salinity <- argo[["salinity"]][, i]
        temperature <- argo[["temperature"]][, i]
        expect_equal(top[["pressure"]][, i], ifelse(pressure < pcut, pressure, NA))
        expect_equal(top[["salinity"]][, i], ifelse(pressure < pcut, salinity, NA))
        expect_equal(top[["temperature"]][, i], ifelse(pressure < pcut, temperature, NA))
    }
})

if (requireNamespace("sf", quietly = TRUE)) {
    test_that("subset(argo, within=(POLYGON))", {
        # Labrador Sea (this test will fail if data(argo) is changed)
        nlevel <- 56
        nold <- 223
        nnew <- 53
        expect_equal(nold, nchar(argo[["direction"]]))
        expect_equal(c(nlevel, nold), dim(argo[["pressure"]]))
        expect_equal(nold, length(argo[["latitude"]]))
        bdy <- list(
            x = c(-41.05788, -41.92521, -68.96441, -69.55673),
            y = c(64.02579, 49.16223, 50.50927, 61.38379)
        )
        argoSubset <- subset(argo, within = bdy)
        expect_equal(nnew, nchar(argoSubset[["direction"]]))
        expect_equal(c(nlevel, nnew), dim(argoSubset[["pressure"]]))
        expect_equal(nnew, length(argoSubset[["latitude"]]))
        expect_equal(c(nlevel, nnew), dim(argoSubset[["salinityFlag"]]))
        expect_equal(c(nlevel, nnew), dim(argoSubset[["temperatureFlag"]]))
        N <- names(argo[["metadata"]])
        N <- c("direction", N[grep("QC$", N)])
        for (item in N) {
            expect_equal(nnew, nchar(argoSubset[[item]]))
        }
    })
}

test_that("preferAdjusted() works on data", {
    a2 <- preferAdjusted(argo) # defaults to which="all"
    expect_equal(a2[["salinity"]], argo@data$salinityAdjusted)
    expect_equal(a2[["temperature"]], argo@data$temperatureAdjusted)
    expect_equal(a2[["pressure"]], argo@data$pressureAdjusted)
    a3 <- preferAdjusted(argo, which = c("salinity"))
    expect_equal(a3[["salinity"]], argo@data$salinityAdjusted)
    expect_equal(a3[["temperature"]], argo@data$temperature)
    expect_equal(a3[["pressure"]], argo@data$pressure)
})

test_that("preferAdjusted() works on flags", {
    a2 <- preferAdjusted(argo) # defaults to which="all"
    expect_equal(a2[["salinityFlags"]], argo@data$flags$salinityAdjusted)
    expect_equal(a2[["temperatureFlags"]], argo@data$flags$temperatureAdjusted)
    expect_equal(a2[["pressureFlags"]], argo@data$flags$pressureAdjusted)
    a3 <- preferAdjusted(argo, which = c("salinity"))
    expect_equal(a2[["salinityFlags"]], argo@data$flags$salinityAdjusted)
    expect_equal(a3[["temperatureFlags"]], argo@data$flags$temperature)
    expect_equal(a3[["pressureFlags"]], argo@data$flags$pressure)
})

test_that("preferAdjusted() works on units", {
    a2 <- preferAdjusted(argo) # defaults to which="all"
    expect_equal(a2[["salinityUnits"]], argo@data$units$salinityAdjusted)
    expect_equal(a2[["temperatureUnits"]], argo@data$units$temperatureAdjusted)
    expect_equal(a2[["pressureUnits"]], argo@data$units$pressureAdjusted)
    a3 <- preferAdjusted(argo, which = c("salinity"))
    expect_equal(a3[["salinityUnits"]], argo@data$units$salinityAdjusted)
    expect_equal(a3[["temperatureUnits"]], argo@data$units$temperature)
    expect_equal(a3[["pressureUnits"]], argo@data$units$pressure)
})

test_that("subset.argo(argo, \"adjusted\") correctly alters metadata and data", {
    a <- subset(argo, "adjusted")
    expect_equal(a@metadata$flags$pressureQC, argo@metadata$flags$pressureAdjustedQC)
    expect_equal(a@metadata$flags$temperatureQC, argo@metadata$flags$temperatureAdjustedQC)
    expect_equal(a@metadata$flags$salinityQC, argo@metadata$flags$salinityAdjustedQC)
    expect_equal(a@metadata$flags$pressure, argo@metadata$flags$pressureAdjusted)
    expect_equal(a@metadata$flags$salinity, argo@metadata$flags$salinityAdjusted)
    expect_equal(a@metadata$flags$temperature, argo@metadata$flags$temperatureAdjusted)
})

test_that("argo [[ handles SA and CT", {
    SA <- argo[["SA"]]
    CT <- argo[["CT"]]
    SP <- argo[["salinity"]]
    t <- argo[["temperature"]]
    p <- argo[["pressure"]]
    lon <- rep(argo[["longitude"]], each = dim(SP)[1])
    lat <- rep(argo[["latitude"]], each = dim(SP)[1])
    expect_equal(SA, gsw_SA_from_SP(SP = SP, p = p, longitude = lon, latitude = lat))
    expect_equal(CT, gsw_CT_from_t(SA = SA, t = t, p = p))
})

test_that("argo name conversion", {
    table <- "BBP BBP
    BETA_BACKSCATTERING betaBackscattering
    BPHASE_DOXY bphaseOxygen
    CDOM CDOM
    CHLA chlorophyllA
    CNDC conductivity
    CP beamAttenuation
    CYCLE_NUMBER cycleNumber
    DOWN_IRRADIANCE downwellingIrradiance
    DOWNWELLING_PAR downwellingPAR
    DOXY oxygen
    FIT_ERROR_NITRATE fitErrorNitrate
    FLUORESCENCE_CDOM fluorescenceCDOM
    FLUORESCENCE_CHLA fluorescenceChlorophyllA
    MOLAR_DOXY oxygenUncompensated
    NITRATE nitrate
    PH_IN_SITU_FREE pHFree
    PH_IN_SITU_TOTAL pH
    PRES pressure
    RAW_DOWNWELLING_IRRADIANCE rawDownwellingIrradiance
    RAW_DOWNWELLING_PAR rawDownwellingPAR
    RAW_UPWELLING_RADIANCE rawUpwellingRadiance
    TEMP temperature
    TEMP_DOXY temperatureOxygen
    TEMP_NITRATE temperatureNitrate
    TEMP_PH temperaturePH
    TEMP_CPU_CHLA temperatureCPUChlA
    TEMP_SPECTROPHOTOMETER_NITRATE temperatureSpectrophotometerNitrate
    TEMP_VOLTAGE_DOXY temperatureVoltageOxygen
    TILT tilt
    TURBIDITY turbidity
    TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION transmittanceParticleBeamAttenuation
    UP_RADIANCE upwellingRadiance
    UV_INTENSITY_DARK_NITRATE UVIntensityDarkNitrate
    UV_INTENSITY_NITRATE UVIntensityNitrate
    VRS_PH pHSensorVoltageDifference
    "

    ## REMAINING TO DO--
    ## see https://github.com/dankelley/oce/issues/1122
    ##
    ## BPHASE_DOXY2
    ## C1PHASE_DOXY
    ## C2PHASE_DOXY
    ## FREQUENCY_DOXY
    ## HUMIDITY_NITRATE
    ## IB_PH
    ## PHASE_DELAY_DOXY
    ## RPHASE_DOXY
    ## TPHASE_DOXY

    a <- read.table(text = table, header = FALSE, stringsAsFactors = FALSE)
    expect_equal(argoNames2oceNames(a$V1), a$V2)
    expect_equal(argoNames2oceNames(paste(a$V1, "123", sep = "")), paste(a$V2, "123", sep = ""))
})

test_that("argo time conversion", {
    expect_equal(1000, timeToArgoJuld(argoJuldToTime(1000)))
})

test_that("[[ handles both cycleNumber and cycle", {
    expect_equal(argo[["cycleNumber"]], 1:223)
    expect_equal(argo[["cycle"]], 1:223)
})

test_that("can get spiciness from 2-column built-in argo file", {
    file <- system.file("extdata", "D4902337_219.nc", package = "oce")
    expect_silent(d <- read.argo(file))
    spiciness0 <- d[["spiciness0"]]
    dim(spiciness0)
    nrow <- 501
    ncol <- 2
    expect_equal(c(nrow, ncol), dim(spiciness0))
    expect_equal(
        head(spiciness0),
        structure(c(
            -0.740665385460943, -0.777904393473634, -0.843097049624618,
            -0.869665638826788, -0.888032049002035, -0.898649902384776, -0.735575985145829,
            -0.737577845207895, -0.73793188835235, -0.739934295750885, -0.741397992241687,
            -0.74540191167246
        ), dim = c(6L, 2L))
    )
    ctd <- as.ctd(d, profile = 1)
    expect_equal(ctd[["spiciness0"]], d[["spiciness0"]][, 1])
})

file <- "local_data/GL_PR_PF_5906438.nc"
if (file.exists(file)) {
    test_that("can read a Copernicus BGC file", {
        expect_silent(d <- read.argo.copernicus(file))
        expect_equal(d[["id"]], "5906438")
        expect_equal(d[["temperature"]][1, 1], 20.5530009762151)
        expect_equal(d[["salinity"]][1, 1], 36.8180017487612)
        expect_equal(d[["pressure"]][1, 1], 4.30000019073486)
        expect_equal(dim(d[["pressure"]]), c(501, 68))
    })
}

file <- "local_data/argo/D4903224_012.nc"
if (file.exists(file)) {
    test_that("can get spiciness from 2-column private (non-CRAN) argo file", {
        expect_silent(d <- read.argo(file))
        spiciness0 <- d[["spiciness0"]]
        expect_equal(c(1011, 2), dim(spiciness0))
        expect_equal(
            head(spiciness0),
            structure(c(
                6.17969497378797, 6.18055246198217,
                6.17894144748325, 6.17736722741175, 6.17343624448158,
                6.15970734970877, 6.18065183201523, 6.18109006131989,
                6.18108469023132, 6.18059622062317, 6.17726061670029,
                6.18102988626372
            ), dim = c(6L, 2L))
        )
        ctd <- as.ctd(d, profile = 1)
        expect_equal(ctd[["spiciness0"]], d[["spiciness0"]][, 1])
    })
}

file <- "local_data/argo/D1901584_124.nc"
if (file.exists(file)) {
    test_that("can get spiciness from 1-column argo file", {
        expect_silent(d <- read.argo(file))
        spiciness0 <- d[["spiciness0"]]
        expect_equal(c(67, 1), dim(spiciness0))
        # consistency check
        expect_equal(
            head(spiciness0),
            structure(c(
                7.05052334875785, 7.04906137175043,
                7.04942780641913, 7.04903685921474, 7.04831733400729,
                7.02504314496063
            ), dim = c(6L, 1L))
        )
        ctd <- as.ctd(d, profile = 1)
        expect_equal(ctd[["spiciness0"]], d[["spiciness0"]][, 1])
    })
}
