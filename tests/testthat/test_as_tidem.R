# vim:textwidth=140:expandtab:shiftwidth=4:softtabstop=4
# See also https://www.github.com/dankelley/oce/issues/2146 (which
# covers more time, and shows a deviation that I do not understand,
# and will keep in mind as I read more about tides).  If I tried
# the present criterion across all time, it would fail,
# because the deviation gets as large as 0.004.

library(oce)

rms <- function(x) sqrt(mean(x^2, na.rm=TRUE))
# predictions from NOAA are given to 0.001 m resolution
eps <- 0.001 # see above, re issue 2146

test_that("Honolulu NOAA predictions are recovered (approximately)", {
    # Honolulu
    # https://tidesandcurrents.noaa.gov/stationhome.html?id=1612340
    h <- read.delim("harmonics.tsv.gz", sep="\t")
    o <- read.csv("predictions.csv.gz")
    o$time <- as.POSIXct(o$Date.Time, tz="UTC")
    # https://tidesandcurrents.noaa.gov/stationhome.html?id=1612340
    latitude <- 21+18.2/60

    expect_message(
        expect_message(
            expect_message(
                expect_message(m <- as.tidem(tRef=mean(o$time), latitude=latitude,
                    name=h$Name, amplitude=h$Amplitude, phase=h$Phase),
                    "converted NOAA name \"LAM2\" to oce name \"LDA2\""),
                "converted NOAA name \"M1\" to oce name \"NO1\""),
            "converted NOAA name \"RHO\" to oce name \"RHO1\""),
        "converted NOAA name \"2MK3\" to oce name \"MO3\"")

    pp <- predict(m, o$time)
    zoffset <- mean(pp - o$Prediction)
    pp <- pp - zoffset
    rms(o$Prediction-pp)
    expect_true(rms(o$Prediction-pp) < eps)
})
