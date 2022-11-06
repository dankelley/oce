library(oce)
source("~/git/oce/R/ctd.R")
#install.packages("changepoint")

tpFull <- function(name, type="p", cex=0.5, pch=20)
{
    data <- get(name)
    png(paste0("jh_", name, "_%d.png"))
    plotScan(data, xtype="time", type=type, cex=cex, pch=pch)
    plot(data[["time"]], data[["salinity"]],
        xlab="Time", ylab="S", type=type, cex=cex, pch=pch)
    plot(data[["time"]], data[["temperature"]],
        xlab="Time", ylab="T", type=type, cex=cex, pch=pch)
    plot(data, eos="unesco")
    dev.off()
}
tp <- function(name, type="p", cex=0.5, pch=20)
{
    data <- get(name)
    plotScan(data, xtype="time", type=type, cex=cex, pch=pch)
}


d0 <- read.oce("SBE19plus_01906009_2019_04_11.cnv")
#tp("d0")

# use plotScan() and then locator(2) to find indices
#d1 <- ctdTrim(d0, method="index", parameters=c(3127,4785))
#tp("d1")

sbe2 <- function(data, parameters=NULL, debug=0)
{
    p <- data$pressure
    if (is.null(parameters))
        parameters <- list()
    if (is.null(parameters$minSoak))
        parameters$minSoak <- 1
    if (is.null(parameters$maxSoak))
        parameters$maxSoak <- 20
    if (is.null(parameters$toleranceSoak))
        parameters$toleranceSoak <- 1
    if (parameters$maxSoak <= parameters$minSoak)
        stop("require minSoak < maxSoak")
    # Find most common pressure that is within the indicated range
    look <- parameters$minSoak <= p & p <= parameters$maxSoak
    h <- hist(p[look], breaks=sum(look)/10, plot=FALSE) # FIXME: 10 is a guess
    pSoak <- h$mids[which.max(h$counts)]
    if (is.null(parameters$toleranceSoak))
        parameters$toleranceSoak <- 1
    pSoakMin <- pSoak - parameters$toleranceSoak
    pSoakMax <- pSoak + parameters$toleranceSoak
    # Find indices of pressures in soaking range
    soaking <- pSoakMin <= p & p <= pSoakMax
    # Debugging plots
    if (debug > 0L) {
        oceDebug(debug, vectorShow(pSoak))
        oceDebug(debug, vectorShow(pSoakMin))
        oceDebug(debug, vectorShow(pSoakMax))
        abline(h=pSoak, col=4)
        abline(h=pSoakMin, col=4)
        abline(h=pSoakMax, col=4)
        abline(h=parameters$minSoak, col=4, lty="dotted")
        abline(h=parameters$maxSoak, col=4, lty="dotted")
    }
    # Use runlengthst find pre-soak and soak intervals at the start
    rleSoak <- rle(soaking)
    startSoak <- rleSoak$lengths[1]
    endSoak <- startSoak + rleSoak$lengths[2]
    if (debug > 0L) {
        oceDebug(debug, vectorShow(startSoak))
        oceDebug(debug, vectorShow(endSoak))
        abline(v=startSoak, col=4)
        abline(v=endSoak, col=4)
    }
    np <- length(p)
    look <- rep(TRUE, np)
    # Ignore pre-soak and soak
    look[1:endSoak] <- FALSE
    # Ignore data after max depth found (i.e. ignore upcast)
    deepest <- which.max(p)
    if (debug > 0L) {
        oceDebug(debug, vectorShow(deepest))
        abline(v=deepest, col=2)
    }
    look[deepest:np] <- FALSE
    # Finally, if it went up to the surface after soaking, trim that also
    shallowest <- which.min(p[look]) + which(look)[1]
    oceDebug(debug, vectorShow(shallowest))
    if (debug > 0L) {
        abline(v=shallowest, col=2)
    }
    look[1:shallowest] <- FALSE
    look
}
plotScan(d0, type="p", pch=20, cex=0.2)
keep <- sbe2(d0@data, debug=1)
points(seq_along(d0[["pressure"]]), d0[["pressure"]],
    pch=20, cex=0.2, col=ifelse(keep, 2, 1))

d1 <- ctdTrim(d0, method=sbe2, parameters=list(minSoak=1, maxSoak=20), debug=1)
plotScan(d1)
plot(d1, eos="unesco")
plotTS(d1, eos="unesco", type="o", pch=20, cex=0.5)

p <- d1[["pressure"]]
t <- d1[["time"]]
dpdt <- diff(p) / diff(as.numeric(t))
dpdt <- c(dpdt[1], dpdt)
plot(t, dpdt, ylab="dp/dt [dbar/s]", type="p", pch=20, cex=0.5)
abline(h=quantile(dpdt, 0.1), col=2)
abline(h=median(dpdt), col=4)
abline(h=mean(dpdt) - sd(dpdt), col=3)
# hist(dpdt, breaks=length(dpdt)/20)

library(changepoint)
cptm <- cpt.var(dpdt, method="PELT", Q=2)
plot(d1[["time"]], cptm)

hist(dpdt, breaks=length(dpdt)/20)
abline(v=median(dpdt), col=2)
abline(v=median(dpdt)+c(-1,1)*sd(dpdt), col=2)
abline(v=0.2, lwd=3)
dpdtSmoothed <- lowpass(dpdt, n=11)
plot(d1[["time"]], dpdtSmoothed, type="o", cex=0.5)
abline(h=median(dpdtSmoothed), col=2)
abline(h=mean(dpdtSmoothed), col=4)
abline(h=mean(dpdtSmoothed)-sd(dpdtSmoothed), col=4)

plot(p, dpdtSmoothed, type="o", cex=0.5)
plot(t[-1], diff(dpdtSmoothed)/diff(as.numeric(t)), type="l", cex=0.5)


