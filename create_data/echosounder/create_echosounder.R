library(oce)
data(echosounder)
echosounderOLD <- echosounder
## I would like to make dataset have dim(ECHOSOUNDER@data$a) = c(736,88) but doing that with decimate()
## does not yield the smoothness in the existing datafile. The present code is useless except perhaps
## to remind me where the original file resided, what times I used for chopping, etc.

##echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_161403.dt4")
files <- c("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4",
           "~/Dropbox/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4")
ok <- FALSE
for (file in files) {
    if (file.exists(file)) {
        message(file)
        echosounder <- read.oce(file)
        ok <- TRUE
        break
    }
}
if (!ok)
    stop("cannot find file in c(", paste(files, collapse=","), ")")

#echosounder <- read.oce("dood/Dropbox/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4")
##echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_164323.dt4")
##echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4")
##echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_164742.dt4")
##echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_165611.dt4")

echosounder <- subset(echosounder, depth < 40)
##echosounder <- subset(echosounder, time > as.POSIXct("2008-07-01 16:44:30", tz="UTC"))
echosounder <- decimate(echosounder, c(2, 40))

expect_equal(echosounder[["a"]][10,10:15],
             c(1101.6125, 818.9500, 892.3500, 1393.0625, 2320.9500, 5840.2750))
expect_equal(echosounder[["a"]][10:15,10],
             c(1101.6125, 1164.5500, 875.3750, 842.5750, 959.6250, 1076.6500))

expect_equal(head(echosounder[["time"]]),
             as.POSIXct(c("2008-07-01 16:39:41.019", "2008-07-01 16:39:41.509",
                          "2008-07-01 16:39:42.000", "2008-07-01 16:39:42.485",
                          "2008-07-01 16:39:42.974", "2008-07-01 16:39:43.464"),
                        tz="UTC"))
expect_equal(head(echosounder[["latitude"]]),
             c(47.87948333, 47.87948333, 47.87948333, 47.87948825, 47.87949642,
               47.87950000))
expect_equal(head(echosounder[["longitude"]]),
             c(-69.72364436, -69.72366061, -69.72367686, -69.72368808,
               -69.72369625, -69.72370900))
e <- echosounder
if (!interactive()) png("echosounder-new.png")
plot(echosounder)#, drawBottom=TRUE)#, despike=TRUE)#, which="zx image")
if (!interactive()) dev.off()
if (!interactive()) png("echosounder-old.png")
plot(echosounderOLD, drawBottom=TRUE, despike=TRUE, which="zx image")
if (!interactive()) dev.off()

## Force to save in version 2, because otherwise users with R 3.5.x and
## earlier will not be able to use data("echosounder")
if (TRUE || utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    save(echosounder, file="echosounder.rda", version=2)
    tools::resaveRdaFiles("echosounder.rda", version=2)
    message("saving in version 2, so R<3.6 can handle data(echosounder)")
} else {
    save(echosounder, file="echosounder.rda")
    tools::resaveRdaFiles("echosounder.rda")
}

