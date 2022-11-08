library(oce)
library(segmented)

sbe2 <- function(data, parameters=NULL)
{
    debug <- 1 # causes messages and plotting
    p <- data$pressure
    if (is.null(parameters))
        parameters <- list()
    if (is.null(parameters$minSoak))
        parameters$minSoak <- 1
    if (is.null(parameters$maxSoak))
        parameters$maxSoak <- 20
    if (is.null(parameters$toleranceSoak))
        parameters$toleranceSoak <- 1
    if (is.null(parameters$N))
        parameters$N <- 200
    # Need descent speed to exceed mean+descentFactor(std dev)
    if (is.null(parameters$descentFactor))
        parameters$descentFactor <- 2
    if (parameters$maxSoak <= parameters$minSoak)
        stop("require minSoak < maxSoak")
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
    cat(vectorShow(mean(sinking)))
    cat(vectorShow(sd(sinking)))
    cat(vectorShow(dp))
    cat(vectorShow(sinkingFast))
    cat(vectorShow(keep))
    plot(p, col=ifelse(sinkingFast, 2, 1), pch=20, cex=0.5, ylim=c(min(p),
            max(p)+5))
    i <- seq_along(p)
    # segmented is very slow for big data, and I don't
    # much like it anyway, on a test as below with file
    # <?>
    #m <- lm(p ~ i)
    #s <- segmented(m, npsi=10)
    #abline(v=unname(s$psi[,2]), col=4, lwd=3)
    cat(vectorShow(parameters))
    pb <- binAverage(i, p, 1.0, np, (np+1.0)/parameters$N)
    pb$y <- lowpass(pb$y, n=5)
    fast <- unname(quantile(diff(pb$y), 0.75))
    cat(vectorShow(fast))
    cat(vectorShow(pb))
    abline(h=fast,col=4)
    lines(pb$x, pb$y, col=2)           # superimpose...
    lines(pb$x, pb$y+3, col=2)         # ... and draw offset
    keep <- keep & sinkingFast
    keep
}
human <- read.csv("analysis_kelley.csv")

for (file in list.files(pattern="cor.*.cnv")) {
    message(file)
    d <- read.oce(file)
    plotScan(d, type="p", pch=20, cex=0.2)
    keep <- sbe2(d@data)
    points(seq_along(d[["pressure"]]), d[["pressure"]],
        pch=20, cex=0.5, col=ifelse(keep, 2, 1))
    # compare human-determined start/end
    w <- which(human$File==file)
    abline(v=human$Start[w], col="magenta", lwd=2, lty="dotted")
    abline(v=human$End[w], col="magenta", lwd=2, lty="dotted")
    mtext(file, line=0.25)
    legend("topleft", bg="white", lwd=1, lty=2, col="magenta",
        legend=c("Human"))
}

