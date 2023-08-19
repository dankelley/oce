# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
#
#
# Test an AD2CP file containing 7 datasets, obtained on 2023-03-06
# from Clark Richards (@richardsc on github). These are 'bench data',
# so we don't try to make sense of the velocities, etc., but focus
# instead mainly on data dimensions.
library(oce)

file <- "local_data/ad2cp/multi_dataset.ad2cp"

if (file.exists(file)) {
    skip_on_cran()
    test_that("file contains 7 data sets",
        {
            expect_message(toc <- read.oce(file, TOC=TRUE),
                "setting plan=")
            # We know the file length from another tool
            expect_equal(length(toc), 7L)
            # We insist on certain names, as a change-limiter.  The numbers were
            # checked by Clark Richards.  I'm only testing two items here.
            expect_equal(toc[[1]],
                structure(list(ID.hex=c("0x16", "0xa0"),
                        ID.dec=c(22L, 160L),
                        dataType=c("average", "text"),
                        Count=c(54L, 2L)),
                    class="data.frame",
                    row.names=c(NA, -2L)))
            expect_equal(toc[[4]],
                structure(list(ID.hex=c("0x16", "0x17", "0xa0"),
                        ID.dec=c(22L, 23L, 160L),
                        dataType=c("average", "bottomTrack", "text"),
                        Count=c(260L, 259L, 1L)),
                    class="data.frame",
                    row.names=c(NA, -3L)))
        })

    test_that("dataSet 4 'average' has v,a,q of correct dimensions",
        {
            expect_message(d <- read.adp.ad2cp(file, dataSet=4, dataType="average"),
                "setting plan=")
            expect_equal(c(260, 15, 4), dim(d[["v"]]))
            expect_equal(c(260, 15, 4), dim(d[["a"]]))
            expect_equal(c(260, 15, 4), dim(d[["q"]]))
        })

    test_that("dataSet 4 'average' v values unchanged from earlier values",
        {
            expect_message(d <- read.adp.ad2cp(file, dataSet=4, dataType="average"),
                "setting plan=")
            expect_equal(d[["v"]][1:10, 1, 1],
                c(-0.612, 0.824, 0.712, 0.944, 0.781, 0.954, 0.339, -0.944, -0.178, 0.580))
            expect_equal(d[["v"]][1:10, 2, 2],
                c(-0.534, -0.335, 0.253, -0.262, -0.584, 0.360, 0.715, -0.995, -0.338, 0.060))
        })

    test_that("dataSet 4 'average' subset by time",
        {
            expect_message(d <- read.adp.ad2cp(file, dataSet=4, dataType="average"),
                "setting plan=0")
            N <- 5
            ds <- subset(d, time < d[["time"]][N+1L])
            expect_equal(ds[["v"]][, 1, 1], c(-0.612, 0.824, 0.712, 0.944, 0.781))
            expect_equal(ds[["v"]][, 2, 2], c(-0.534, -0.335, 0.253, -0.262, -0.584))
            # every item is a vector, a matrix, or an array
            for (name in names(ds@data)) {
                if (!(name %in% c("distance"))) {
                    item <- ds@data[[name]]
                    if (is.vector(item)) {
                        expect_equal(N, length(item))
                    } else if (is.matrix(item)) {
                        expect_equal(N, dim(item)[1])
                    } else if (is.array(item)) {
                        expect_equal(N, dim(item)[1])
                    }
                }
            }
        })

    test_that("dataSet 4 'average' subset by distance",
        {
            expect_message(d <- read.adp.ad2cp(file, dataSet=4, dataType="average"),
                "setting plan=0")
            N <- 5
            ds <- subset(d, distance < d[["distance"]][N+1L])
            # every item is a vector, a matrix, or an array
            for (name in names(ds@data)) {
                if (!(name %in% c("accelerometer", "magnetometer"))) {
                    item <- ds@data[[name]]
                    if (is.matrix(item) && !(name %in% c("accelerometer", "magnetometer"))) {
                        expect_equal(N, dim(item)[2])
                    } else if (is.array(item)) {
                        expect_equal(N, dim(item)[2])
                    }
                }
            }
        })

    test_that("dataSet 4 'bottomTrack'",
        {
            expect_message(d <- read.adp.ad2cp(file, dataSet=4, dataType="bottomTrack"),
                "setting plan=0")
            expect_equal(c(259, 4), dim(d[["v"]]))
            expect_equal(c(259, 4), dim(d[["figureOfMerit"]]))
        })

    test_that("dataSet 4 from, to, by",
        {
            expect_message(d1 <- read.adp.ad2cp(file, dataSet=4, dataType="average"),
                "setting plan=")
            expect_message(d2 <- read.adp.ad2cp(file, dataSet=4, dataType="average",
                    from=1, to=10, by=2),
                "setting plan=")
            s <- seq(1, 10, 2)
            expect_equal(c(5, 15, 4), dim(d2[["v"]]))
            expect_equal(c(5, 15, 4), dim(d2[["a"]]))
            expect_equal(c(5, 15, 4), dim(d2[["q"]]))
            expect_equal(d1[["time"]][s], d2[["time"]])
            expect_equal(d1[["v"]][s, , ], d2[["v"]][, , ])
            expect_equal(d1[["q"]][s, , ], d2[["q"]][, , ])
            expect_equal(d1[["a"]][s, , ], d2[["a"]][, , ])
        })

}
