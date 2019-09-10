## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("webtide")

if (file.exists("/data/nwatl")) {
  test_that("webtide", {
            path <- paste(getOption("webtide"), "/data/nwatl", sep="")
            pattern <- "nwatl_ll.nod"
            if (1 == length(list.files(path=path, pattern=pattern))) {
              a <- webtide("predict", longitude=-63, latitude=44, plot=FALSE)
              expect_equal(names(a), c("time", "elevation", "u", "v", "node", "basedir","region"))
              b <- webtide("map", longitude=-63, latitude=44, plot=FALSE)
              expect_equal(names(b), c("node", "latitude", "longitude"))
              ## The next three tests will fail if webtide is updated and the numbers
              ## change, so they are blocked out, but still left here, in case
              ## the developer may find them helpful.
              if (FALSE) {
                expect_equal(head(a$elevation), c(-0.20609221409, -0.14463463411, -0.08172047958,
                                                  -0.01835870715, 0.04443627478, 0.10566059067))
                expect_equal(head(a$u), c(0.0069110978571, 0.0053063933931, 0.0038722056401,
                                          0.0026186134050, 0.0015517477196, 0.0006737364902))
                expect_equal(head(a$v), c(0.04802913583, 0.04822143736, 0.04790889414,
                                          0.04709160408, 0.04577651402, 0.04397733928))
              }
            }
})
}
