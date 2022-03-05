# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)

if (file.exists("local_data/itp99grd0000.dat")) {
    test_that("ice-tethered profiler", {
        itp <- read.ctd.itp("local_data/itp99grd0000.dat")
        expect_equal(itp[["latitude"]], 77.8840)
        expect_equal(itp[["longitude"]], 360 + (-145.0856))
})}

if (file.exists("local_data/18HU2010014_00003_00001_ct1.csv")) {
    test_that("woce 1", {
        woce <- read.ctd.woce("local_data/18HU2010014_00003_00001_ct1.csv")
        expect_equal(woce[["longitude"]], -52.5945)
        expect_equal(woce[["latitude"]], 47.5483)
        expect_equal(woce[["station"]], 3)
})}

if (file.exists("local_data/example_ct1.csv")) {
    test_that("woce 2", {
        woce <- read.ctd.woce("local_data/example_ct1.csv")
        expect_equal(woce[["latitude"]], -17.5102)
        expect_equal(woce[["longitude"]], -150.4812)
        expect_equal(woce[["station"]], 221)
        expect_equal(woce[["waterDepth"]], 3596)
        expect_equal(woce[["pressureUnit"]], list(unit=expression(dbar), scale=""))
        expect_equal(woce[["temperatureUnit"]], list(unit=expression(degree*C), scale="ITS-90"))
        expect_equal(woce[["salinityUnit"]], list(unit=expression(), scale="PSS-78"))
        expect_equal(woce[["oxygenUnit"]], list(unit=expression(mu*mol/kg), scale=""))
})}

# I dump files here sometimes, when I download new data that seem to provide
# useful test cases.
if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("various ctd files", {
        files <- c("77DN20020420_hy1.csv",
            "p10_00026_00001_ct1.csv",
            "sr01_l_00001_00003_ct1.csv",
            "p02_2004a_00175_00002_ct1.csv",
            "i06sb_00062_00001_ct1.csv",
            "a23_00043_00001_ct1.csv",
            "a22_00025_00001_ct1.csv",
            "a03_3_00001_ct1.csv",
            "a22_2003a_00001_00001_ct1.csv",
            "18HU2010014_00003_00001_ct1.csv",
            "18HU20130507_00235_00001_ct1.csv")
        for (file in files) {
            #> cat(file, "\n")
            if (file == "18HU20130507_00235_00001_ct1.csv")
                expect_warning(d <- read.oce(paste("local_data", file, sep="/")),
                    "missingValue inferred as -999 from S and T minima")
            else
                d <- read.oce(paste("local_data", file, sep="/"))
            #> ## summarizing and plotting can depend on the data, so try both
            #> summary(d)
            #> plot(d)
        }
})}

if (file.exists("local_data/CTD_98911_1P_1_DN.txt")) {
    test_that("a broken ODF file that has theta but no S", {
        d <- read.oce("local_data/CTD_98911_1P_1_DN.txt")
        # Until 2021-12-21, we were extracting 'theta' directly from
        # this broken file (which contains pressure, temperature,
        # and theta, but no salinity).  On this day, though, it was
        # decided to drop this special-case handling.  After all,
        # it is a broken file.  Anyone needing to work with this could
        # presumably go back to the source.  Besides, the user
        # can always just do d@data$theta to get the data, if they
        # really want that. The other factor in the decision is
        # that there could be dozens of special cases that might
        # need to be checked, and what's the point in confusing
        # readers by making guesses for all of them?  (What formula
        # for 'theta' was even used in this case?)
        #
        #20211121 expect_equal(length(d[["theta"]]), 127)
        #20211121 expect_equal(head(d[['theta']]), c(0.0346, 0.1563, 0.2153, 0.1970, 0.1916, 0.2141))
        expect_error(d[["theta"]], "the object's data slot lacks 'salinity'")
})}

if (1 == length(list.files(path=".", pattern="local_data"))) {
    test_that("autoconverts pressure in PSI to in dbar", {
        # test creation of pressure [dbar] from pressure [PSI], using
        # the constructed file ctd_with_psi.cnv (in which the
        # pressure column was calculated and inserted into the file,
        # and in which also the header line was changed to say that
        # pressure is in English units.
        expect_warning(
            expect_warning(
                d1 <- read.oce("local_data/ctd.cnv"),
                "this CNV file has temperature in the IPTS-68 scale"),
            "startTime < 1950, suggesting y2k problem in this cnv file")
        expect_warning(
            expect_warning(
                expect_warning(
                    d2 <- read.oce("local_data/ctd_with_psi.cnv"),
                    "created 'pressure' from 'pressurePSI'"),
                "this CNV file has temperature in the IPTS-68 scale"),
            "startTime < 1950, suggesting y2k problem in this cnv file")
        # use 1e-5 to reflect the number of digits I was using in
        # creating and then cut/pasting the fake data
        expect_equal(d1[["pressure"]], d2[["pressure"]], tolerance=1e-5)
})}

