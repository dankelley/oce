## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
lon <- -60
lat <- -30
t <- seq(as.POSIXct("2017-08-30"), as.POSIXct("2017-09-01"), by="15 min")
tsec <- as.numeric(t)
u <- sin(tsec * 2 * pi / (3600 * 12.4206))
v <- 0.5 * cos(tsec * 2 * pi / (3600 * 12.4206))

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  plot(t, u, type="l")
#  lines(t, v, col=2)

## -----------------------------------------------------------------------------
library(oce)
o <- new("oce")

## -----------------------------------------------------------------------------
str(o)

## -----------------------------------------------------------------------------
o <- oceSetData(o, "t", t, list(unit=expression(s), scale=""))
o <- oceSetData(o, "u", u, list(unit=expression(m/s), scale=""))
o <- oceSetData(o, "v", v, list(unit=expression(m/s), scale=""))

## -----------------------------------------------------------------------------
o <- oceSetMetadata(o, "longitude", lon)
o <- oceSetMetadata(o, "latitude", lat)

## -----------------------------------------------------------------------------
summary(o)

## -----------------------------------------------------------------------------
o[["latitude"]]                        # from metadata
head(o[["t"]])                         # from data
head(o[["u"]])
head(o[["v"]])

## ----fig.cap="**Figure 11.** Crude plot of constructed `oce` object.", fig.width=4, fig.height=4, dpi=72----
plot(o)

## -----------------------------------------------------------------------------
setClass("uv", contains="oce")

## -----------------------------------------------------------------------------
setMethod(f="initialize",
          signature="uv",
          definition=function(.Object, time, u, v, longitude, latitude) {
              if (missing(time)) stop("must provide 'time'")
              ## repeat the above also for u, v, longitude, and latitude
              .Object@metadata$units$u <- list(unit=expression(m/s), scale="")
              .Object@metadata$units$v <- list(unit=expression(m/s), scale="")
              .Object@metadata$longitude <- longitude
              .Object@metadata$latitude <- latitude
              .Object@data$time <- time
              .Object@data$u <- u
              .Object@data$v <- v
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'uv' object"
              return(.Object)
          })

## -----------------------------------------------------------------------------
oo <- new("uv", t, u, v, lon, lat)

## -----------------------------------------------------------------------------
setMethod(f="summary",
          signature="uv",
          definition=function(object, ...) {
              cat("uv Summary\n-----------\n\n", ...)
              cat(paste("* Location:           ",
                        sprintf("%.5f N", object@metadata$latitude), ", ",
                        sprintf("%.5f E", object@metadata$longitude), "\n", sep=''))
              callNextMethod()
          })

## -----------------------------------------------------------------------------
summary(oo)

## -----------------------------------------------------------------------------
setMethod(f="plot",
          signature=signature("uv"),
          definition=function(x, which=1, ...)
          {
              if (which == 1) {
                  oce.plot.ts(x[["time"]], x[["u"]], ylab="u [m/s]", ...)
              } else if (which == 2) {
                  oce.plot.ts(x[["time"]], x[["v"]], ylab="v [m/s]", ...)
              } else if (which == 3) {
                  plot(x[["u"]], x[["v"]], xlab="u [m/s]", ylab="v [m/s]", ...)
              }
          })

## ----echo=FALSE---------------------------------------------------------------
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0)) # thin margins

## ----fig.cap="**Figure 12.** Time-series u(t) plot of newly-constructed `uv` object.", fig.width=4, fig.height=4, dpi=72----
par(mfrow=c(2,1))
plot(oo, which=1)
plot(oo, which=2)

## ----fig.cap="**Figure 13.** Hodograph plot of newly-constructed `uv` object.", fig.width=4, fig.height=4, dpi=72----
plot(oo, which=3, asp=1)

## -----------------------------------------------------------------------------
library(oce)
wave <- setClass(Class="wave", contains="oce")

## -----------------------------------------------------------------------------
w <- new("wave")

## -----------------------------------------------------------------------------
class(w)

## -----------------------------------------------------------------------------
inherits(w, "wave")

## -----------------------------------------------------------------------------
str(w)

## -----------------------------------------------------------------------------
w[["metadata"]]

## -----------------------------------------------------------------------------
w[["data"]]

## -----------------------------------------------------------------------------
w[["metadata"]]$station <- "STN01"

## -----------------------------------------------------------------------------
str(w)

## -----------------------------------------------------------------------------
w <- oceSetMetadata(w, "serialNumber", 1234)

## -----------------------------------------------------------------------------
str(w[["metadata"]])

## -----------------------------------------------------------------------------
t <- as.POSIXct("2019-01-01 00:00:00", tz="UTC") + seq(0, 30, length.out=100)
tau <- 10
e <- sin(as.numeric(2 * pi * as.numeric(t) / tau)) + rnorm(t, sd=0.01)

## -----------------------------------------------------------------------------
w <- oceSetData(w, "time", t)
w <- oceSetData(w, "elevation", e)

## -----------------------------------------------------------------------------
summary(w)

## -----------------------------------------------------------------------------
w <- oceSetData(w, "elevation", e, unit=list(unit=expression(m),scale=""))

## -----------------------------------------------------------------------------
summary(w)

## -----------------------------------------------------------------------------
setMethod(f="plot",
    signature=signature("wave"),
    definition=function(x, which=1, ...) {
        if (which == 1) {
            plot(x[["time"]], x[["elevation"]], ...)
        } else if (which == 2) {
            hist(x[["elevation"]], ...)
        } else {
            stop("which must be 1 or 2")
        }
    })

## -----------------------------------------------------------------------------
plot(w)

## -----------------------------------------------------------------------------
plot(w, type="l", xlab="Time [s]", ylab="Elevation [m]")

## -----------------------------------------------------------------------------
setMethod(f="initialize",
    signature="wave",
    definition=function(.Object, time, elevation, units) {
        if (missing(units)) {
            .Object@metadata$units <- list()
            if (missing(units))
                .Object@metadata$units$elevation <- list(unit=expression(m), scale="")
        }
        .Object@data$time <- if (missing(time)) NULL else time
        .Object@data$elevation <- if (missing(elevation)) NULL else elevation
        .Object@processingLog$time <- presentTime()
        .Object@processingLog$value <- "create 'wave' object"
        return(.Object)
    }
)

## -----------------------------------------------------------------------------
ww <- new("wave", time=t, elevation=e)
summary(ww)

## -----------------------------------------------------------------------------
setMethod(f="[[",
    signature(x="wave", i="ANY", j="ANY"),
    definition=function(x, i, j, ...) {
        if (i == "peak") {
           return(max(x[["elevation"]], na.rm=TRUE))
        } else {
           callNextMethod()
        }
    }
)

## -----------------------------------------------------------------------------
w[["peak"]]
str(w[["elevation"]])

