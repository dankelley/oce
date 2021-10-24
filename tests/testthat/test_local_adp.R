# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)

f <- "~/Dropbox/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/adp_sontek_h53.adp"
if (file.exists(f)) {
    test_that("Sontek adp", {
        n <- 5000
        adp <- read.oce(f, from=n+1, to=n+3)
        # numeric time comparison is easier for these whacky times
        expect_equal(as.numeric(adp[["time"]])-1214438000,
            c(0.5599999428, 10.6500000954, 20.6400001049))

        v <- c(0.51, 0.51, 0.51, 0.756, 0.756, 0.756, 0.17, 0.17, 0.17,
            0.1, 0.1, 0.1, 1.7, 1.7, 1.7, 0.227, 0.227, 0.227, 0.05,
            0.05, 0.05, 1.5, 1.5, 1.5, 0.123, 0.123, 0.123, 0.123,
            0.123, 0.123, 0.123, 0.123, 0.123, 0, 0, 0, 1.114, 1.435,
            1.174, 0.477, 0.622, 0.029, -0.052, 0.011, -0.092, 0, 0,
            0, 20.554, 19.021, 20.302, 0.074, 0.079, 0.08, -0.978,
            -0.686, -1.154, -1.054, -0.764, -1.215, -1.173, -0.872,
            1.218, 1.223, -0.939, 1.172, 1.186, -1.005, 1.131, 1.141,
            -1.103, 1.107, 1.136, -1.171, 1.063, 1.12, -1.216, 1.024,
            1.074, 1.232, 1.041, 1.048, 1.207, 1.061, 1.013, 1.191,
            1.047, 0.971, 1.133, 1.052, 0.98, 1.102, 1.023, 0.976,
            1.084, 1.026, 0.977, 1.06, 1.017, 0.975, 1.03, 1.029,
            0.998, 0.996, 1.001, 1.002, 0.974, 0.975, 0.989, 0.884,
            0.947, 0.951, 0.866, 0.887, 0.97, 0.863, 0.855, 0.952,
            0.607, 0.831, 0.982, 0.656, 0.803, 0.986, 0.565, 0.749,
            0.969, 0.499, 0.713, 0.972, 0.462, 0.714, 0.959, 0.466,
            0.683, 0.882, 0.446, 0.607, 0.787, 0.44, 0.562, 0.696,
            0.455, 0.514, 0.609, 0.534, 0.46, 0.324, 0.269, 0.229,
            0.928, 1.068, 0.335, 0.716, 0.877, 0.222, 0.592, 0.698,
            0.194, 0.486, 0.546, 0.126, 0.428, 0.482, 0.068, 0.401,
            0.469, 0.024, 0.365, 0.394, 0.043, 0.346, 0.381, 0.002,
            0.314, 0.303, -0.055, 0.299, 0.294, -0.075, 0.285, 0.263,
            -0.057, 0.271, 0.312, -0.049, 0.237, 0.33, -0.015, 0.201,
            0.328, 0.015, 0.224, 0.348, 0.057, 0.234, 0.343, 0.041,
            0.219, 0.344, 0.09, 0.133, 0.316, 0.077, 0.155, 0.076,
            0.116, 0.25, 0.289, 0.096, 0.34, 0.294, 0.048, 0.31,
            0.268, 0.077, 0.354, 0.237, 0.077, 0.336, 0.232, 0.073,
            0.349, 0.204, -0.019, 0.365, 0.262, -0.077, 0.302, 0.136,
            0.013, 0.139, 0.001, -0.139, 0.308, -0.022, 0.016, 0.249,
            -0.045, -0.043, 0.317, -0.02, -0.04, 0.259, -0.003,
            -0.059, 0.315, 0.348, 0.291, 0.182, 0.221, 0.156, 0.084,
            0.131, 0.045, 0.026, 0.079, -0.01, -0.048, 0.031, -0.061,
            -0.075, -0.007, -0.102, -0.142, -0.086, -0.123, -0.184,
            -0.107, -0.14, -0.17, -0.12, -0.163, -0.164, -0.149,
            -0.141, -0.191, -0.129, -0.128, -0.202, -0.146, -0.134,
            -0.191, -0.123, -0.135, -0.217, -0.124, -0.132)
        dim(v) <- c(3, 32, 3)
        expect_equal(adp[["v"]], v)
        expect_equal(adp[["distance"]], c(0.05, 0.09, 0.13, 0.17, 0.21,
                0.25, 0.29, 0.33, 0.37, 0.41,
                0.45, 0.49, 0.53, 0.57, 0.61,
                0.65, 0.69, 0.73, 0.77, 0.81,
                0.85, 0.89, 0.93, 0.97, 1.01,
                1.05, 1.09, 1.13, 1.17, 1.21,
                1.25, 1.29))
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Teledyn/RDI read (integer from,to) and check", {
        beam <- read.oce("local_data/adp_rdi",
            from=1, to=10, latitude=47.88126, longitude=-69.73433)
        xyz <- beamToXyzAdp(beam)
        enu <- xyzToEnuAdp(xyz, declination=-18.1)
        expect_equal(c(10, 84, 4), dim(beam[["v"]]))
        expect_equal(c(10, 84, 4), dim(xyz[["v"]]))
        expect_equal(c(10, 84, 4), dim(enu[["v"]]))
        expect_equal(beam[["latitude"]], 47.88126)
        expect_equal(xyz[["latitude"]], 47.88126)
        expect_equal(enu[["latitude"]], 47.88126)
        expect_equal(beam[["longitude"]], -69.73433)
        expect_equal(xyz[["longitude"]], -69.73433)
        expect_equal(enu[["longitude"]], -69.73433)
        # FIXME: add more tests on the data
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Teledyn/RDI read (POSIXct from,to)", {
        beam <- read.oce("local_data/adp_rdi",
            from=as.POSIXct("2008-06-25 10:01:00",tz="UTC"),
            to=as.POSIXct("2008-06-25 10:03:00",tz="UTC"))
        expect_true(is.na(beam[["latitude"]]))
        expect_true(is.na(beam[["longitude"]]))
        expect_equal(dim(beam[["v"]]), c(13,84,4))
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Teledyn/RDI binmap", {
        beam <- read.oce("local_data/adp_rdi",
            from=1, to=10, latitude=47.88126, longitude=-69.73433)
        expect_silent(beam2 <- binmapAdp(beam))
        # FIXME: add tests on the data
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Nortek aquadopp read and check", {
        beam <- read.oce("local_data/adp_nortek_aquadopp",
            from=1, to=10, latitude=47.87943, longitude=-69.72533)
        xyz <- beamToXyzAdp(beam)
        enu <- xyzToEnuAdp(xyz, declination=-18.1)
        expect_equal(c(10, 25, 3), dim(beam[["v"]]))
        expect_equal(c(10, 25, 3), dim(xyz[["v"]]))
        expect_equal(c(10, 25, 3), dim(enu[["v"]]))
        expect_equal(beam[["latitude"]], 47.87943)
        expect_equal(xyz[["latitude"]], 47.87943)
        expect_equal(enu[["latitude"]], 47.87943)
        expect_equal(beam[["longitude"]], -69.72533)
        expect_equal(xyz[["longitude"]], -69.72533)
        expect_equal(enu[["longitude"]], -69.72533)
        # FIXME: add more tests on the data
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Sontek (PCADP)", {
        beam <- read.oce("local_data/adp_sontek",
            from=1, to=10, latitude=48.87961, longitude=-69.72706)
        expect_equal(48.87961, beam[["latitude"]])
        expect_equal(-69.72706, beam[["longitude"]])
        expect_equal(dim(beam[["v"]]), c(10, 32, 3))
        beam2 <- read.oce("local_data/adp_sontek",
            from=as.POSIXct("2008-06-25 10:00:40", tz="UTC"),
            to=as.POSIXct("2008-06-25 10:01:30", tz="UTC"),
            latitude=48.87961, longitude=-69.72706)
        expect_equal(dim(beam2[["v"]]), c(6, 32, 3))
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Teledyne/RDI Sentinel V subset by pressure", {
        expect_warning(
            expect_warning(
                expect_warning(
                    expect_output(
                        d <- read.oce("local_data/adp_sentinel_v.pd0"),
                        "Got to end of data"),
                    "skipping the first ensemble"),
                "A list of unhandled segment codes"),
            "so trimming time")
        keep <- d[["pressure"]] < median(d[["pressure"]])
        dsp <- subset(d, pressure < median(d[["pressure"]]))
        expect_equal(sum(keep), dim(dsp[["v"]])[1])
        expect_equal(sum(keep), length(dsp[["time"]]))
        for (slant in c("v", "a")) { # "g" is NULL
            expect_equal(sum(keep), dim(dsp[[slant]])[1])
        }
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("Teledyne/RDI Sentinel V subset by distance", {
        expect_warning(
            expect_warning(
                expect_warning(
                    expect_output(
                        d <- read.oce("local_data/adp_sentinel_v.pd0"),
                        "Got to end of data"),
                    "skipping the first ensemble"),
                "A list of unhandled segment codes"),
            "so trimming time")
        keepSlant <- sum(d[["distance"]] < median(d[["distance"]]))
        keepVertical <- sum(d[["vdistance"]] < median(d[["distance"]]))
        dsd <- subset(d, distance < median(d[["distance"]]))
        expect_equal(keepSlant, length(dsd[["distance"]]))
        expect_equal(keepVertical , length(dsd[["vdistance"]]))
        for (vert in c("va", "vq", "vv")) { # no "vg" in this dataset
            expect_equal(keepVertical, dim(dsd[[vert]])[2])
        }
        for (slant in c("v", "a")) { # "g" is NULL
            expect_equal(keepSlant, dim(dsd[[slant]])[2])
        }
})}

