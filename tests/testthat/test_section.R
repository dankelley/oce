# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)
data("argo")
data(section)                          # 124 stations (slow for tests)
eastern <- subset(section, longitude < (-72.5)) # 8 stations (faster)

# Many of these tests will fail if data(section) is changed. This is on
# purpose, because changing a long-standing dataset is to be avoided!

test_that("data(section) has not altered", {
    expect_equal(124, length(section[["station"]]))
    expect_equal(124, length(section@data$station))
    expect_equal(8, length(eastern[["station"]]))
    expect_equal(8, length(eastern@data$station))
})

test_that("section[['z']] == -section[['depth']]", {
    z <- eastern[["z"]]
    depth <- eastern[["depth"]]
    expect_equal(z, -depth)
})

# Next takes 3.6 s with full dataset, 0.38 s with eastern dataset
test_that("section[[...]] and [[..., \"byStation\"]] work", {
    for (i in c("CT", "depth", "nitrate", "nitrite", "oxygen",
            "phosphate", "potential temperature", "pressure", "SA",
            "salinity", "sigmaTheta", "silicate", "spice",
            "temperature", "theta", "z")) {
        v <- eastern[[i]]
        expect_true(is.vector(v))
        expect_equal(length(v), 151)
        l <- eastern[[i, "byStation"]]
        expect_true(is.list(l))
        expect_equal(head(v, 24), l[[1]])
        expect_equal(length(l), 8)
    }
})

test_that("as.section() data-quality flags (consistency check)", {
    # These tests are against values known to be in the file. Essentially,
    # we are checking reading, accessing, and subsetting.
    # The below is also in ../../create_data/section/check_section.R, and it would be
    # smart to update both at the same time.
    stn8 <- eastern[["station", 8]]
    # there are no flags on temperature or pressure
    expect_equal(stn8[["startTime"]], as.POSIXct("1993-10-25 09:53:00", tz="UTC"))
    expect_equal(stn8@metadata$flags$salinity, c(3, 3, 3, 2, 2, 2, 2, 2))
    expect_equal(stn8@metadata$flags$salinityBottle, c(2, 2, 2, 2, 2, 2, 2, 2))
    expect_equal(stn8@metadata$flags$oxygen, c(2, 2, 2, 2, 2, 2, 2, 2))
    expect_equal(stn8@metadata$flags$silicate, c(2, 2, 2, 2, 3, 2, 2, 2))
    expect_equal(stn8@metadata$flags$nitrite, rep(2, 8))
    expect_equal(stn8@metadata$flags[["NO2+NO3"]], c(5, 5, 5, 2, 2, 2, 2, 2))
    expect_equal(stn8@metadata$flags$phosphate, rep(2, 8))
    expect_equal(stn8[["nitrite"]], c(0.01, 0.05, 0.05, 0.07, 0.25, 0.03, 0.02, 0.02))
})

test_that("section station extraction", {
    expect_equal(length(eastern@data$station), length(eastern[["station"]]))
    expect_equal(eastern[["station", 1]][["station"]], "126")
    expect_equal(eastern[["station", "126"]][["station"]], "126")
})

test_that("as.section() works with names of CTD objects", {
    data(ctd)
    fake <- ctd
    fake[["temperature"]] <- ctd[["temperature"]] + 0.5
    fake[["salinity"]] <- ctd[["salinity"]] + 0.1
    fake[["longitude"]] <- ctd[["longitude"]] + 0.01
    fake[["station"]] <- "fake"
    sec <- as.section(c("ctd", "fake"))
    expect_equal(2, length(sec[["station"]]))
})

test_that("as.section() works with vector of CTD objects", {
    data(ctd)
    ctds <- vector("list", 2)
    ctds[[1]] <- ctd
    fake <- ctd
    fake[["temperature"]] <- ctd[["temperature"]] + 0.5
    fake[["salinity"]] <- ctd[["salinity"]] + 0.1
    fake[["longitude"]] <- ctd[["longitude"]] + 0.01
    fake[["station"]] <- "fake"
    ctds[[2]] <- fake
    expect_warning(sec <- as.section(ctds), "estimated waterDepth as max\\(pressure\\) for CTDs numbered 1:2")
    expect_equal(2, length(sec[["station"]]))
})

test_that("as.section() works with argo object", {
    sec <- as.section(subset(argo, profile < 10))
    expect_equal(9, length(sec[["station"]]))
})

test_that("subset(section, indices=(NUMERIC))", {
    twoStations <- subset(eastern, indices=2:3)
    expect_equal(2, length(twoStations[["station"]]))
    expect_true(identical(eastern[["station", 3]], twoStations[["station", 2]]))
})

test_that("subset(section, indices=(LOGICAL))", {
    long <- subset(eastern,
        indices=unlist(lapply(eastern[["station"]], function(s) 10<length(s[["pressure"]]))))
    expect_equal(7, length(long[["station"]]))
    expect_equal(eastern[["station",2]], long[["station",2]])
})

test_that("subset(section, longitude < (NUMERIC))", {
    secWest <- subset(section, longitude < -50)
    expect_lt(max(secWest[["longitude"]]), -50)
})

test_that("subset(section, pressure < 2000)", {
    top2km <- subset(section, pressure < 2000) # drops stn 56 and 62
    section100 <- section[["station", 100]]
    top2km98 <- top2km[["station", 98]]
    expect_equal(tail(section100[["pressure"]]), c(3530.9, 3746.1, 3970.6, 4189.5, 4346.7, 4398.3))
    expect_equal(tail(top2km98[["pressure"]]), c(777.1, 926.5, 1189.5, 1590.1, 1699.8, 1859.5))
})

test_that("subset(section, pressure > 1000)", {
    deep <- subset(section, pressure > 1000) # drops stn 1, 2, 123, 124
    w <- which(section[["station", 100]][["pressure"]] > 1000)
    d <- data.frame(section[["station", 100]][["data"]])[w, ]
    rownames(d) <- 1:dim(d)[1]
    expect_equal(d, as.data.frame(deep[["station", 98]][["data"]]))
})

test_that("subset(section, min(pressure)<100)", {
    SEC <- subset(section, min(pressure) < 100)
    ptop <- unlist(lapply(section[["station"]],
            function(s) min(s[["pressure"]])))
    bad <- sum(ptop >= 100)
    expect_equal(length(SEC[["station"]]), length(section[["station"]]) - bad)
})

test_that("subset(section, length(pressure) > 5)", {
    data(section)
    SEC <- subset(section, length(pressure) > 5)
    plen <- unlist(lapply(section[["station"]],
            function(s) length(s[["pressure"]])))
    bad <- sum(plen <= 5)
    expect_equal(length(SEC[["station"]]), length(section[["station"]]) - bad)
})


test_that("sectionSort", {
    data(section)
    expect_equal(section[["stationId"]],
        c("3", "4", "6", "7", "8", "9", "10", "12", "13", "14",
            "15", "16", "17", "18", "19", "20", "21", "22", "23",
            "24", "25", "26", "28", "29", "30", "32", "33", "34",
            "35", "36", "37", "38", "39", "40", "41", "42", "43",
            "44", "45", "46", "47", "48", "49", "50", "51", "52",
            "53", "54", "55", "56", "57", "58", "59", "60", "61",
            "62", "63", "64", "65", "66", "67", "68", "69", "71",
            "72", "74", "75", "76", "77", "78", "79", "80", "81",
            "82", "83", "84", "85", "86", "87", "88", "89", "90",
            "91", "92", "93", "94", "95", "96", "97", "98", "99",
            "100", "101", "102", "103", "104", "106", "107", "108",
            "109", "110", "111", "112", "113", "114", "115", "116",
            "117", "118", "119", "120", "121", "122", "123", "124",
            "125", "126", "127", "128", "129", "130", "131", "132",
            "133"))
    ss <- sectionSort(section)
    expect_equal(ss[["stationId"]],
        c("10", "100", "101", "102", "103", "104", "106", "107",
            "108", "109", "110", "111", "112", "113", "114", "115",
            "116", "117", "118", "119", "12", "120", "121", "122",
            "123", "124", "125", "126", "127", "128", "129", "13",
            "130", "131", "132", "133", "14", "15", "16", "17",
            "18", "19", "20", "21", "22", "23", "24", "25",
            "26", "28", "29", "3", "30", "32", "33", "34",
            "35", "36", "37", "38", "39", "4", "40", "41",
            "42", "43", "44", "45", "46", "47", "48", "49",
            "50", "51", "52", "53", "54", "55", "56", "57",
            "58", "59", "6", "60", "61", "62", "63", "64",
            "65", "66", "67", "68", "69", "7", "71", "72",
            "74", "75", "76", "77", "78", "79", "8", "80",
            "81", "82", "83", "84", "85", "86", "87", "88",
            "89", "9", "90", "91", "92", "93", "94", "95",
            "96", "97", "98", "99"))
})

test_that("stationReplaceIndividualStation", {
    eastern[["station"]][[1]] <- "not a CTD object"
    expect_equal(eastern@data$station[[1]], "not a CTD object")
})

test_that("stationReplaceAllStations", {
    expect_false("N2" %in% names(eastern[["station",1]][["data"]]))
    eastern[["station"]] <- lapply(eastern[["station"]], function(x) oceSetData(x, "N2", swN2(x)))
    expect_true("N2" %in% names(eastern[["station",1]][["data"]]))
})

test_that("sectionGrid units and flags", {
    # Work with a subset for speed of test.
    sg <- sectionGrid(eastern, p=seq(0, 5000, 500))
    # Check flag names (in this dataset, all stations have same flags)
    expect_equal(sort(names(section[["station",1]][["flags"]]), method="radix"),
        sort(names(sg[["station",1]][["flags"]]), method="radix"))
    # Check units (in this dataset, all stations have same units)
    expect_equal(eastern[["station",1]][["units"]], sg[["station",1]][["units"]])
})

# Next takes 1.8 s with section and 1km, 0.09 s with eastern and 10km.
test_that("sectionSmooth grid extends past data (issue 1583)", {
    # xgrid extends past data, owing to the ceiling(). This caused
    # an error (reported as issue 1583) prior to 2019 July 19.
    expect_silent(sectionSmooth(eastern, "barnes",
            xg=seq(0, ceiling(max(eastern[['distance', 'byStation']])), by=10),
            yg=seq(5, ceiling(max(eastern[['pressure']])), by=25)))
})

test_that("sectionSmooth units and flags", {
    data(section)
    # Work with a subset for speed of test.
    s <- subset(section, 115<=stationId&stationId<=121)
    # NOTE: there's no need to check other methods besides "spline",
    # because units and flags are handled in code that applies to all
    # methods.
    sspline <- sectionSmooth(sectionGrid(s, p=seq(0, 5000, 500)), "spline")
    # Check flag names (in this dataset, all stations have same flags)
    expect_equal(sort(names(sspline[["station",1]][["flags"]]), method="radix"),
        sort(names(s[["station",1]][["flags"]]), method="radix"))
    # Check units (in this dataset, all stations have same units)
    expect_equal(sspline[["station",1]][["units"]], s[["station",1]][["units"]])
})

test_that("sectionSmooth by spline", {
    sg <- sectionGrid(eastern, p=seq(0, 5000, 500))
    sspline <- sectionSmooth(sg, "spline")
    expect_equal(length(sg[["station"]]), length(sspline[["station"]]))
    # Check dimensionality when xg is given
    sspline2 <- sectionSmooth(sg, "spline", xg=seq(0,200,50))
    expect_equal(length(sspline2[["station"]]), 3)
})

test_that("sectionSmooth by barnes", {
    sbarnes <- sectionSmooth(eastern, "barnes", xr=50, yr=200)
    expect_equal(length(eastern[["station"]]), length(sbarnes[["station"]]))
})

# 2022-03-19. I am commenting this out, since things run differently
# on different machines.  I spent many hours trying to tailor
# this to work for different machines, but I think the underlying
# code (automap or sp, not sure which) is too much in a state of flux
# to be reliable across machines.  Perhaps I ought to revisit this
# after 6 months or so.
#2022-03-19 test_that("sectionSmooth krige (not run)", {
#2022-03-19     if (requireNamespace("automap", quietly=TRUE) && requireNamespace("sp", quietly=TRUE)) {
#2022-03-19         expect_warning(skrigingInternal <- sectionSmooth(eastern, "kriging"))#, "Remove")
#2022-03-19         expect_equal(length(eastern[["station"]]), length(skrigingInternal[["station"]]))
#2022-03-19         expect_warning(skrigingInternal2 <- sectionSmooth(eastern, "kriging", xg=seq(0,200,50)))
#2022-03-19         expect_equal(length(skrigingInternal2[["station"]]), 3)
#2022-03-19         krigFunction <- function(x, y, F, xg, xr, yg, yr) {
#2022-03-19             xy <- data.frame(x=x/xr, y=y/yr)
#2022-03-19             K <- automap::autoKrige(F~1, remove_duplicates=TRUE,
#2022-03-19                 input_data=sp::SpatialPointsDataFrame(xy, data.frame(F)),
#2022-03-19                 new_data=sp::SpatialPoints(expand.grid(xg/xr, yg/yr)))
#2022-03-19             matrix(K$krige_output@data$var1.pred, nrow=length(xg), ncol=length(yg))
#2022-03-19         }
#2022-03-19         owarn <- options("warn")$warn
#2022-03-19         options(warn=-1)
#2022-03-19         expect_output(skrigingUser <- sectionSmooth(eastern, krigFunction))
#2022-03-19         options(warn=owarn)
#2022-03-19         expect_equal(length(skrigingUser[["station"]]), length(s[["station"]]))
#2022-03-19         options(warn=-1)
#2022-03-19         expect_output(skrigingUser2 <- sectionSmooth(eastern, krigFunction, xg=seq(0, 200, 50)))
#2022-03-19         options(warn=owarn)
#2022-03-19         expect_equal(length(skrigingUser2[["station"]]), 3)
#2022-03-19     }
#2022-03-19 })

test_that("lon360 works as intended", {
    data(section)
    sectionShifted <- lon360(section)
    expect_equal(360 + section[["longitude"]], sectionShifted[["longitude"]])
    expect_equal(c(179, 181), lon360(c(179, -179)))
})

