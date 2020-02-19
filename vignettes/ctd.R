## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----fig.cap="**Figure 2.** An overview of a ctd dataset.", fig.width=6, fig.height=6, dpi=72, dev.args=list(pointsize=14), message=FALSE----
library(oce)
data(ctd)
summary(ctd)
plot(ctd)

## ----fig.cap="**Figure 3.** Scanwise plot of the `ctdRaw` sample data set.  Note the spike at the start, the equilibration phase before the downcast, and the spurious freshening signal near the start of the upcast.", fig.width=5, fig.height=3, dpi=72, dev.args=list(pointsize=12)----
data(ctdRaw)
plotScan(ctdRaw)

## ----eval=FALSE---------------------------------------------------------------
#  plotScan(ctdTrim(ctdRaw, "range",
#                   parameters=list(item="scan", from=140, to=250)))
#  plotScan(ctdTrim(ctdRaw, "range",
#                   parameters=list(item="scan", from=150, to=250)))

## ----eval=FALSE---------------------------------------------------------------
#  ctdTrimmed <- ctdTrim(ctdRaw)

## ----eval=FALSE---------------------------------------------------------------
#  plot(ctdDecimate(ctdTrim(read.ctd("stn123.cnv"))))

## ----eval=FALSE---------------------------------------------------------------
#  library(oce)
#  # http://cchdo.ucsd.edu/data/7971/ar18_58JH19941029_ct1.zip
#  # setwd("~/Downloads/ar18_58JH19941029_ct1")
#  files <- system("ls *.csv", intern=TRUE)
#  for (i in 1:length(files)) {
#      x <- read.ctd(files[i])
#      if (i == 1) {
#          plotTS(x, Slim=c(31, 35.5), Tlim=c(-2, 10), type='o')
#      } else {
#          points(x[["salinity"]], x[["potential temperature"]])
#          lines(x[["salinity"]], x[["potential temperature"]])
#      }
#  }

## ----fig.width=5, fig.height=5, fig.keep="none"-------------------------------
library(oce)
data(ctd)
pycnocline <- ctdTrim(ctd, "range",
                      parameters=list(item="pressure", from=5, to=12))
plotProfile(pycnocline, which="density+N2")

## ----fig.width=5, fig.height=5, fig.keep="none"-------------------------------
library(oce)
data(ctd)
pycnocline <- subset(ctd, 5 <= pressure & pressure <= 12)
plotProfile(pycnocline, which="density+N2")

## ----eval=FALSE---------------------------------------------------------------
#  library(oce)
#  # http://cchdo.ucsd.edu/data/7971/ar18_58JH19941029_ct1.zip
#  # setwd("~/Downloads/ar18_58JH19941029_ct1")
#  files <- system("ls *.csv", intern=TRUE)
#  n <- length(files)
#  ctds <- vector("list", n) # to hold the CTD objects
#  station <- vector("list", n)
#  for (i in 1:n) {
#      ctds[[i]] <- read.ctd(files[i])
#      station[[i]] <- ctds[[i]][["station"]]
#  }
#  S <- unlist(lapply(1:n, function(i) ctds[[i]][["salinity"]]))
#  T <- unlist(lapply(1:n, function(i) ctds[[i]][["temperature"]]))
#  p <- unlist(lapply(1:n, function(i) ctds[[i]][["pressure"]]))
#  overall <- as.ctd(S, T, p)
#  png("ar18_%02d.png")
#  for (i in 1:n) {
#      plotTS(overall, col='gray')
#      lines(ctds[[i]][["salinity"]], ctds[[i]][["potential temperature"]])
#      mtext(station[i], side=3, line=0)
#  }
#  dev.off()

