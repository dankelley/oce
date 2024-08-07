# vim:textwidth=140:expandtab:shiftwidth=4:softtabstop=4
library(oce)
CRATwarning <- "\"conductivity\" \\(code name \"CRAT_01\"\\)" # portion of the warning


test_that("argument existence", {
    data(ctd)
    expect_error(
        initializeFlagScheme(ctd, mapping = list(unknown = 1, good = 2, bad = 3)),
        "must supply 'name'"
    )
    expect_error(
        initializeFlagScheme(ctd, name = "unknown"),
        "must supply 'mapping' for new scheme named \"unknown\""
    )
})

test_that("handleFlags() with flags/data in sublist", {
    o <- new("oce")
    o@data[["A"]] <- list(x = 1:3, y = 11:13)
    o@metadata$flags[["A"]] <- list(x = c(2, 4, 2), y = c(2, 4, 2))
    of <- handleFlags(o, flags = c(4), actions = c("NA"), where = "A")
    expect_equal(of[["data"]]$A$x, c(1, NA, 3))
    expect_equal(of[["data"]]$A$y, c(11, NA, 13))
})

test_that("handleFlags() with unnamed list flags", {
    # Does handleFlags work with both variable-specific and overall flags?
    data(section)
    # STN100: multiple flags
    STN100 <- section[["station", 100]]
    deep <- STN100[["pressure"]] > 1500
    flag <- ifelse(deep, 7, 2) # flag deep data as bad
    for (flagName in names(STN100@metadata$flags)) {
        STN100@metadata$flags[[flagName]] <- flag
    }
    STN100f <- handleFlags(STN100)
    # Test just those data that have flags in original object
    for (field in c(
        "salinity", "salinityBottle", "oxygen", "silicate",
        "nitrite", "NO2+NO3", "phosphate"
    )) {
        expect_equal(STN100f[[field]][!deep], STN100[[field]][!deep])
        expect_true(all(is.na(STN100f[[field]][deep])))
    }
    # Test *all* data
    stn100 <- section[["station", 100]]
    stn100@metadata$flags <- list(flag)
    stn100f <- handleFlags(stn100)
    for (field in names(stn100@data)) { # Note: this is *all* the data
        expect_equal(stn100f[[field]][!deep], stn100[[field]][!deep])
        expect_true(all(is.na(stn100f[[field]][deep])))
    }
    expect_equal(stn100[["data"]], STN100[["data"]])
})

test_that("handleFlags() with unnamed vector flags", {
    # Does handleFlags work with both variable-specific and overall flags?
    data(section)
    # STN100: multiple flags
    STN100 <- section[["station", 100]]
    deep <- STN100[["pressure"]] > 1500
    flag <- ifelse(deep, 7, 2) # flag deep data as bad
    for (flagName in names(STN100@metadata$flags)) {
        STN100@metadata$flags[[flagName]] <- flag
    }
    STN100f <- handleFlags(STN100)
    # Test just those data that have flags in original object
    for (field in c(
        "salinity", "salinityBottle", "oxygen", "silicate",
        "nitrite", "NO2+NO3", "phosphate"
    )) {
        expect_equal(STN100f[[field]][!deep], STN100[[field]][!deep])
        expect_true(all(is.na(STN100f[[field]][deep])))
    }
    # Test *all* data
    stn100 <- section[["station", 100]]
    stn100@metadata$flags <- flag
    stn100f <- handleFlags(stn100)
    for (field in names(stn100@data)) { # Note: this is *all* the data
        expect_equal(stn100f[[field]][!deep], stn100[[field]][!deep])
        expect_true(all(is.na(stn100f[[field]][deep])))
    }
    expect_equal(stn100[["data"]], STN100[["data"]])
})

test_that("predefined flag schemes", {
    # DEVELOPER NOTE: keep in synch with R/AllClass.R and man-roxygen/initializeFlagScheme.R
    data(ctd)
    a <- initializeFlagScheme(ctd, "argo")
    expect_equal(
        a[["flagScheme"]],
        list(
            name = "argo",
            mapping = list(
                not_assessed = 0, passed_all_tests = 1,
                probably_good = 2, probably_bad = 3, bad = 4, changed = 5,
                not_used_6 = 6, not_used_7 = 7, estimated = 8, missing = 9
            ),
            default = c(0, 3, 4, 9)
        )
    )
    a <- initializeFlagScheme(ctd, "BODC")
    expect_equal(
        a[["flagScheme"]],
        list(
            name = "BODC",
            mapping = list(
                no_quality_control = 0, good = 1, probably_good = 2,
                probably_bad = 3, bad = 4, changed = 5, below_detection = 6,
                in_excess = 7, interpolated = 8, missing = 9
            ),
            default = c(0, 2, 3, 4, 5, 6, 7, 8, 9)
        )
    )
    a <- initializeFlagScheme(ctd, "DFO")
    expect_equal(
        a[["flagScheme"]],
        list(
            name = "DFO",
            mapping = list(
                no_quality_control = 0, appears_correct = 1, appears_inconsistent = 2,
                doubtful = 3, erroneous = 4, changed = 5,
                qc_by_originator = 8, missing = 9
            ),
            default = c(0, 2, 3, 4, 5, 8, 9)
        )
    )
    a <- initializeFlagScheme(ctd, "WHP bottle")
    expect_equal(
        a[["flagScheme"]],
        list(
            name = "WHP bottle",
            mapping = list(
                no_information = 1, no_problems_noted = 2, leaking = 3,
                did_not_trip = 4, not_reported = 5, discrepency = 6,
                unknown_problem = 7, did_not_trip = 8, no_sample = 9
            ),
            default = c(1, 3, 4, 5, 6, 7, 8, 9)
        )
    )
    a <- initializeFlagScheme(ctd, "WHP CTD")
    expect_equal(
        a[["flagScheme"]],
        list(
            name = "WHP CTD",
            mapping = list(
                not_calibrated = 1, acceptable = 2, questionable = 3,
                bad = 4, not_reported = 5, interpolated = 6,
                despiked = 7, missing = 9
            ),
            default = c(1, 3, 4, 5, 6, 7, 9)
        )
    )
})

test_that("user-created flag scheme", {
    data(ctd)
    a <- initializeFlagScheme(ctd, "myscheme",
        mapping = list(unknown = 1, good = 2, bad = 3),
        default = c(1, 3, 4, 5, 6, 7, 9)
    )
    expect_equal(a[["flagScheme"]], list(
        name = "myscheme",
        mapping = list(unknown = 1, good = 2, bad = 3),
        default = c(1, 3, 4, 5, 6, 7, 9)
    ))
})

test_that("cannot alter existing flag scheme (unless using update arg)", {
    data(ctd)
    ctd1 <- initializeFlagScheme(ctd, "myscheme", list(unknown = 1, good = 2, bad = 3))
    expect_warning(defaultFlags(ctd1), "unable to determine default flags")
    expect_warning(
        ctd2 <- initializeFlagScheme(ctd1, "WHP CTD"),
        "cannot alter a flagScheme that is already is place"
    )
    expect_warning(defaultFlags(ctd2), "unable to determine default flags")
    expect_silent(ctd3 <- initializeFlagScheme(ctd1, "WHP CTD", update = TRUE))
    expect_equal(c(1, 3, 4, 5, 6, 7, 9), defaultFlags(ctd3))
})

test_that("ctd flag scheme action", {
    data(ctd)
    a <- initializeFlags(ctd, "temperature", 2) # 2="acceptable
    expect_warning(initializeFlags(a, "temperature", 2), "cannot re-initialize flags")
    a <- setFlags(a, "temperature", 1:3, 4) # 4="bad"
})

test_that("[[ and [[<- with ctd flags", {
    data(section)
    ctd <- section[["station", 100]]
    expect_equal(c(2, 2, 2, 2, 2, 3), ctd[["salinityFlag"]][1:6])
    ctd[["salinity"]][2] <- -999
    ctd[["salinityFlag"]] <- ifelse(ctd[["salinity"]] < 0, 3, ctd[["salinityFlag"]])
    expect_equal(c(2, 3, 2, 2, 2, 3), ctd[["salinityFlag"]][1:6])
    ctd[["salinity"]] <- ifelse(ctd[["salinityFlag"]] != 2, NA, ctd[["salinity"]])
    expect_equal(is.na(ctd[["salinity"]][1:6]), c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("handleFLags with ctd data", {
    data(section)
    ctd <- section[["station", 100]]
    # this stn has a few points with salinityFlag==3
    ctdNew <- handleFlags(ctd, flags = list(salinity = c(1, 2, 3, 4, 5, 6, 7, 9)))
    # cat("ctd salinity: orig had", sum(is.na(ctd[['salinity']])), "NA values; new has",
    #    sum(is.na(ctdNew[['salinity']])), "\n")
    expect_equal(sum(is.na(ctd[["salinity"]])), 0)
    nbad <- sum(ctd[["salinityFlag"]] != 2)
    expect_equal(2, nbad)
    # test replacement via function
    f <- function(object) rep(30, length.out = length(object[["salinity"]]))
    ctdNew2 <- handleFlags(ctd, flags = list(salinity = 4:5), actions = list(salinity = f))
    expect_equal(
        sum(ctdNew[["salinity"]] == 30, na.rm = TRUE),
        sum(ctd[["salinityFlag"]] == 4 | ctd[["salinityFlag"]] == 5, na.rm = TRUE)
    )
})

test_that("handleFLags with the built-in argo dataset", {
    data(argo)
    argoNew <- handleFlags(argo, flags = list(salinity = c(0, 3:9)))
    # Test a few that are identified by printing some values
    # for argo[["salinityFlag"]].
    expect_true(is.na(argoNew[["salinity"]][13, 2]))
    expect_true(is.na(argoNew[["salinity"]][53, 8]))
    # Test whether data with salinity flag of 4 get changed to NA
    expect_true(all(is.na(argoNew[["salinity"]][4 == argo[["salinityFlag"]]])))
    expect_true(!all(is.na(argoNew[["salinity"]][1 == argo[["salinityFlag"]]])))
    # Similar for temperature. First, check that it is *not* NA, with
    # the call to handleFlags() above, which was restricted to salinity.
    expect_true(!is.na(argoNew[["temperature"]][10, 2]))
    # Now, handle *all* the flags, and check temperature again, and also salinity.
    argoNew2 <- handleFlags(argo, flags = list(4:5))
    expect_true(is.na(argoNew2[["temperature"]][10, 2]))
    expect_true(all(is.na(argoNew2[["temperature"]][4 == argo[["temperatureFlag"]]])))
    # Tests of overall numbers
    expect_equal(sum(is.na(argo[["salinity"]])), 106)
    expect_equal(sum(is.na(argoNew[["salinity"]])), 140)
    # test replacement via function
    f <- function(object) rep(30, length.out = length(object[["salinity"]]))
    argoNew3 <- handleFlags(argo, flags = list(salinity = 4:5), actions = list(salinity = f))
    expect_equal(
        sum(argoNew3[["salinity"]] == 30, na.rm = TRUE),
        sum(argo[["salinityFlag"]] == 4 | argo[["salinityFlag"]] == 5, na.rm = TRUE)
    )
})

test_that("handleFLags with the built-in section dataset", {
    data(section)
    SECTION <- handleFlags(section, flags = list(salinity = c(1, 3:9)))
    # Inspection reveals that salinity are triggered in the first CTD entry, i.e.
    # the station named "3" in this dataset.
    # The default for `handleFlags,ctd-method` is the WOCE standard, with 2=good, 3=bad, ...
    stn1 <- section[["station", 1]]
    STN1 <- SECTION[["station", 1]]
    expect_equal(c(2, 3, 3, 2, 2), stn1[["salinityFlag"]])
    ok <- 2 == stn1[["salinityFlag"]]
    expect_equal(!is.na(STN1[["salinity"]]), ok)
})

test_that("ctd flag with subset() (issue 1410)", {
    data(section)
    stn <- section[["station", 100]]
    stnTopKm <- subset(stn, pressure < 1000)
    n <- length(stnTopKm[["temperature"]])
    for (flag in names(stnTopKm[["flags"]])) {
        flagName <- paste(flag, "Flag", sep = "")
        expect_equal(stnTopKm[[flagName]], head(stn[[flagName]], n))
    }
})

test_that("odf flag with subset() (issue 1410)", {
    file <- system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package = "oce")
    expect_warning(odf <- read.odf(file), CRATwarning)
    # # Find a region with interesting flags
    # > which(odf[["sigmaThetaFlag"]]!=1)
    # [1] 110 120 121 142
    # > which(odf[["salinityFlag"]]!=1)
    # [1] 121
    iStart <- 100
    iEnd <- 130
    sub <- subset(odf, scan[iStart] <= scan & scan <= scan[iEnd])
    n <- length(sub[["temperature"]])
    for (name in names(sub[["flags"]])) {
        flagName <- paste(name, "Flag", sep = "")
        expect_equal(sub[[flagName]], odf[[flagName]][iStart:iEnd])
    }
    for (namei in names(sub[["data"]])) {
        expect_equal(sub[[name]], odf[[name]][iStart:iEnd])
    }
})


test_that("adp flag with subset() (issue 1410)", {
    data(adp)
    v <- adp[["v"]]
    f <- array(FALSE, dim = dim(v))
    updraft <- adp[["v"]][, , 4] > 0
    updraft[is.na(updraft)] <- FALSE # I don't like NA flags
    for (beam in 1:4) {
        f[, , beam] <- updraft
    }
    adp[["vFlag"]] <- f
    # Subset by distance.
    sub <- subset(adp, distance < 20)
    expect_equal(dim(sub[["v"]]), dim(sub[["vFlag"]])) # flag dim = data dim?
    look <- adp[["distance"]] < 20
    expect_equal(adp[["vFlag"]][, look, ], sub[["vFlag"]]) # flag values ok?
    # Subset by time.
    sub <- subset(adp, time <= adp[["time"]][10])
    expect_equal(dim(sub[["v"]]), dim(sub[["vFlag"]])) # flag dim = data dim?
    look <- adp[["time"]] <= adp[["time"]][10]
    expect_equal(adp[["vFlag"]][look, , ], sub[["vFlag"]]) # flag values ok?
})

test_that("initializeFlagScheme with section", {
    data(section)
    expect_equal(
        section[["station", 1]][["flagScheme"]],
        list(
            name = "WHP bottle",
            mapping = list(
                no_information = 1, no_problems_noted = 2, leaking = 3,
                did_not_trip = 4, not_reported = 5, discrepency = 6,
                unknown_problem = 7, did_not_trip = 8, no_sample = 9
            ),
            default = c(1, 3, 4, 5, 6, 7, 8, 9)
        )
    )
})

test_that("handleFlags default flags (section)", {
    data(section)
    # "WHP bottle" scheme used in "section": good=2; bad or questionable=c(1,3:9)
    S1 <- handleFlags(section)
    S2 <- handleFlags(section, flags = c(1, 3:9))
    for (i in seq_along(S1[["station"]])) {
        expect_equal(S1[["station", i]], S1[["station", i]])
    }
})

test_that("alter flag scheme", {
    data(section)
    ctd <- section[["station", 1]]
    expect_equal(c(1, 3:9), defaultFlags(ctd))
    expect_warning(
        ctd <- initializeFlagScheme(ctd, "will give error"),
        "cannot alter a flagScheme that is already is place"
    )
    ctd[["flagScheme"]] <- NULL
    ctd <- initializeFlagScheme(ctd, "argo")
    expect_equal(c(0, 3, 4, 9), defaultFlags(ctd))
})

test_that("handleFlags default flags (ctd)", {
    # use first station of data(section) because data(ctd) has no flags
    data(section)
    ctd <- section[["station", 1]]
    expect_equal(c(1, 3:9), defaultFlags(ctd))
    C1 <- handleFlags(ctd)
    C2 <- handleFlags(ctd, flags = c(1, 3:9))
    expect_equal(C1@data, C2@data)
    expect_equal(C1@metadata, C2@metadata)
})

test_that("adp handleFlag gives error for raw data (issue 1914)", {
    data(adp)
    v <- adp[["v"]]
    i2 <- array(FALSE, dim = dim(v))
    g <- adp[["g", "numeric"]]
    # Thresholds on percent "goodness" and error "velocity"
    G <- 25
    V4 <- 0.45
    for (k in 1:3) {
        i2[, , k] <- ((g[, , k] + g[, , 4]) < G) | (v[, , 4] > V4)
    }
    # Can apply flags to velocity, because it is numeric
    a <- initializeFlags(adp, "v", 2)
    b <- setFlags(a, "v", i2, 3)
    expect_silent(c <- handleFlags(b, flags = list(3), actions = list("NA")))
    # Cannot apply flags to amplitude, because it is raw
    a <- initializeFlags(adp, "a", 2)
    b <- setFlags(a, "a", i2, 3)
    expect_error(
        c <- handleFlags(b, flags = list(3), actions = list("NA")),
        "use adpConvertRawToNumeric"
    )
})
