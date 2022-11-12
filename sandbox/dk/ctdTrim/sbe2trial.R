method <- 2
if (!interactive()) pdf("sbe2trial.pdf")
PARAM <- list(A=1/5, B=1/2)
par(mar=c(3,3,1,1), mgp=c(1.5,0.5,0))
# Ideas
#
# 1. Chop the pressure time-series into N segments (default 10) and fit
# linearly in each. (Fitting is fast and does not involve choices that would be
# required for smoothing with a spline, or doing lowpass filtering.)
#
# 2. Check to see if some segments have negative slopes, there have an upcast
# to be removed.  Find it as max(p), perhaps after smoothing, and chop all
# points after it.  Now we have a downcast at the end of the interval, and
# possibly a soaking intervale before it.
#
# 3. Check for a soaking interval by finding a histogram of pressure in the 2
# to 20 dbar range.  If this lasts for a reasonable time, as indicated by a
# fraction of total sampling time, or a fixed length of time) then it must be
# chopped.  To do that, take the most common pressure value, extend it by a
# reasonable amount, and find the last point in that interval.  (Some smoothing
# will likely be required.) Then move forward until a local minimum is found,
# and erase all data before that.
#
# 4. At this stage there ought to be a downcast, perhaps with plateaus regions
# before and after.  Remove these regions by computing the mean delta-p within
# the region and removing points with values that are smaller by a factor of X.

library(oce)
file <- "SBE19plus_01906009_2019_04_11.cnv"

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


#d0 <- read.oce(files[1]"SBE19plus_01906009_2019_04_11.cnv")
#d0 <- read.oce("~/git/ctdTrim/data/SSBE19plus_01906009_2019_04_11")
#tp("d0")

# use plotScan() and then locator(2) to find indices
#d1 <- ctdTrim(d0, method="index", parameters=c(3127,4785))
#tp("d1")

sbe2 <- function(data, parameters=NULL, filename="")
{
    debug <- 1 # causes messages and plotting
    p <- data$pressure
    np <- length(p)
    i <- seq_len(np)
    if (is.null(parameters))
        parameters <- list()
    if (is.null(parameters$minSoak))
        parameters$minSoak <- 1
    if (is.null(parameters$maxSoak))
        parameters$maxSoak <- 20
    if (is.null(parameters$toleranceSoak))
        parameters$toleranceSoak <- 1
    # Need descent speed to exceed mean+descentFactor(std dev)
    if (is.null(parameters$descentFactor))
        parameters$descentFactor <- 2
    if (parameters$maxSoak <= parameters$minSoak)
        stop("require minSoak < maxSoak")
    #
    if (method == 2) {
        NNN <- 500
        if (np < NNN) {
            tmp <- approx(i, p, seq(1, np, length.out=NNN))
            I <- tmp$x
            P <- lowpass(tmp$y, n=5)
            message("short")
        } else {
            AVG <- binAverage(i, p, 1, np, np/NNN)
            I <- AVG$x
            P <- lowpass(AVG$y, n=11)
            message("long")
        }
        #plot(ba$x,jitter(ba$y,amount=5),col=ifelse(c(0,diff(ba$y)>0),2,1))
        par(mfrow=c(1,1))
        plot(i, p, type="l", col="gray")
        lines(I, P, col=2)
        mtext(filename)
        dP <- diff(P)
        dP <- c(P[1], dP)
        M <- quantile(abs(dP), 0.95) # do not use absolute max because of noise
        par(mfrow=c(2,1))
        plot(I, dP, cex=1/3, type="o")
        abline(h=M*c(-1,1), col="magenta", lty="8393")
        mtext(filename)
        plot(I, dP/M, cex=1/3, type="o")
        abline(h=PARAM$A*c(-1,1), col=4)
        abline(h=PARAM$B*c(-1,1), col=2)
        mtext("blue:hover, red:rise/sink")
        keep <- parameters$minSoak <= P & P <= parameters$maxSoak
        h <- hist(P[keep], breaks=sum(keep)/10, plot=FALSE) # FIXME: 10 is a guess
        pSoak <- h$mids[which.max(h$counts)]
        if (is.null(parameters$toleranceSoak))
            parameters$toleranceSoak <- 1
        pSoakMin <- pSoak - parameters$toleranceSoak
        pSoakMax <- pSoak + parameters$toleranceSoak
        # Find indices of pressures in soaking range
        soaking <- pSoakMin <= P & P <= pSoakMax
        hovering <- abs(dP)/M < PARAM$A
        sinking <- dP/M > PARAM$B
        rising <- dP/M < -PARAM$B
        OFF <- 2
        par(mfrow=c(2,2))
        plot(I, P, type="l", col="gray")
        points(I[soaking], P[soaking]+OFF, col=2, cex=0.3)
        points(I[!soaking], P[!soaking]-OFF, col=4, cex=0.3)
        mtext("soaking", line=-1, adj=0.01, cex=par("cex"))
        mtext(filename, cex=par("cex"))
        plot(I, P, type="l", col="gray")
        points(I[hovering], P[hovering]+OFF, col=2, cex=0.3)
        points(I[!hovering], P[!hovering]-OFF, col=4, cex=0.3)
        mtext("hovering", line=-1, adj=0.01, cex=par("cex"))
        plot(I, P, type="l", col="gray")
        points(I[rising], P[rising]+OFF, col=2, cex=0.3)
        points(I[!rising], P[!rising]-OFF, col=4, cex=0.3)
        mtext("rising", line=-1, adj=0.01, cex=par("cex"))
        plot(I, P, type="l", col="gray")
        points(I[sinking], P[sinking]+OFF, col=2, cex=0.3)
        points(I[!sinking], P[!sinking]-OFF, col=4, cex=0.3)
        mtext("sinking", line=-1, adj=0.01, cex=par("cex"))
        return()
    }


    # Find most common pressure that is within the indicated range
    keep <- parameters$minSoak <= p & p <= parameters$maxSoak
    h <- hist(p[keep], breaks=sum(keep)/10, plot=FALSE) # FIXME: 10 is a guess
    pSoak <- h$mids[which.max(h$counts)]
    if (is.null(parameters$toleranceSoak))
        parameters$toleranceSoak <- 1
    pSoakMin <- pSoak - parameters$toleranceSoak
    pSoakMax <- pSoak + parameters$toleranceSoak
    # Find indices of pressures in soaking range
    soaking <- pSoakMin <= p & p <= pSoakMax
    cat(vectorShow(sum(soaking)))
    cat(vectorShow(np))
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
    # Use runlength to find pre-soak and soak intervals at the start
    rleSoak <- rle(soaking)
    startSoak <- rleSoak$lengths[1]
    endSoak <- startSoak + rleSoak$lengths[2]
    cat(vectorShow(sum(soaking)))
    cat(vectorShow(endSoak))
    cat(vectorShow(sum(soaking)/endSoak))
    cat(vectorShow(sum(soaking)/np))
    if (debug > 0L) {
        oceDebug(debug, vectorShow(startSoak))
        oceDebug(debug, vectorShow(endSoak))
        abline(v=startSoak, col=4)
        abline(v=endSoak, col=4)
    }
    np <- length(p)
    keep <- rep(TRUE, np)
    # Ignore pre-soak and soak
    keep[1:endSoak] <- FALSE
    # Ignore data after max depth found (i.e. ignore upcast)
    deepest <- which.max(p)
    if (debug > 0L) {
        oceDebug(debug, vectorShow(deepest))
        abline(v=deepest, col=2)
    }
    keep[deepest:np] <- FALSE
    # Trim ascent from soaking depth to the surface
    shallowest <- which.min(p[keep]) + which(keep)[1]
    oceDebug(debug, vectorShow(shallowest))
    if (debug > 0L) {
        abline(v=shallowest, col=2)
    }
    keep[1:shallowest] <- FALSE
    # Trim slow descents
    dp0 <- smooth(diff(p[keep]))
    sinking <- dp0[dp0 > 0]
    #hist(sinking, breaks=50);abline(v=mean(sinking),col=4)
    #abline(v=quantile(sinking), col=2)
    #message(vectorShow(mean(sinking)))
    #message(vectorShow(sd(sinking)))
    #message(vectorShow(mean(sinking)-sd(sinking)))
    #message(vectorShow(mean(sinking)-2*sd(sinking)))
    #abline(v=mean(sinking)-2*sd(sinking), lwd=3)
    #plot(sinking > mean(sinking)-2*sd(sinking))
    dp <- diff(smooth(p))
    dp <- c(dp[1], dp)
    sinkingFast <- dp > mean(sinking) - parameters$descentFactor * sd(sinking)
    #message(vectorShow(dp))
    #message(vectorShow(sinkingFast))
    #message(vectorShow(look))
    keep <- keep & sinkingFast
    # extra ideas for later:
    cat(vectorShow(sum(dp > 0) / length(dp)))
    SINKING <- lowpass(dp,n=101) > 0#median(dp)/50
    x <- seq_along(p)[SINKING]
    y <- p[SINKING]
    points(x,y,col=4, cex=0.3)
    DAN<<-list(p=p,dp=dp,x=x,y=y,SINKING=SINKING)
    keep
}
#plotScan(d0, type="p", pch=20, cex=0.2)

files <- c("SBE19plus_01906009_2019_04_11.cnv", "cor2017080_preproc.cnv")
d <- lapply(files, read.oce)

keep <- sbe2(d[[1]]@data, filename=files[1])
keep <- sbe2(d[[2]]@data, filename=files[2])

#points(seq_along(d0[["pressure"]]), d0[["pressure"]],
#    pch=20, cex=0.5, col=ifelse(keep, 2, 1))
if (!interactive()) dev.off()
stop()

# compare human-determined start/end
human <- read.csv("analysis_dk.csv")
w <- which(df$File==file)
abline(v=df$Start[w], col="magenta", lwd=2, lty="dotted")
abline(v=df$End[w], col="magenta", lwd=2, lty="dotted")

stop()

d1 <- ctdTrim(d0, method=sbe2, parameters=list(minSoak=1, maxSoak=20), debug=1)
plotScan(d1)
plot(d1, eos="unesco")
plotTS(d1, eos="unesco", type="o", pch=20, cex=0.5)

p <- d1[["pressure"]]
t <- d1[["time"]]
dpdt <- diff(p) / smooth(diff(as.numeric(t)))
dpdt <- c(dpdt[1], dpdt)
plot(t, dpdt, ylab="dp/dt [dbar/s]", type="o", pch=20, cex=0.5)
abline(h=quantile(dpdt, 0.1), col=2)
abline(h=median(dpdt), col=4)
abline(h=mean(dpdt) - sd(dpdt), col=3)
# hist(dpdt, breaks=length(dpdt)/20)

#library(changepoint)
#cptm <- cpt.var(dpdt, method="PELT", Q=2)
#plot(d1[["time"]], cptm)

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


