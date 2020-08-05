library(oce)
readComponent <- function(file)
{
    ## The data format is described on page 5 (PDF page 12) of Foreman (1978).
    ##
    ## Foreman, M. G. G. “Manual for Tidal Currents Analysis and Prediction.”
    ## Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
    ## Sciences, Patricia Bay, 1978.
    widths <- c(2, 5, 9, 2, 2, rep(5, 12))
    names <- c("KOLI", "JSTN", "ID", "IM", "IY", paste("KARD", 1:12, sep=""))
    d <- utils::read.fwf(file, widths=widths, col.names=names)
    npair <- dim(d)[1]
    if (0 != npair %% 2)
        stop("file '", file, "' does not  contain an even number of lines")
    ## String together paired lines
    t <- NULL                              # time [POSIXct]
    velo <- NULL                           # velocity component [m/s]
    day <- ISOdatetime(1900+d$IY, d$IM, d$ID, 0, 0, 0, tz="UTC")
    for (i in seq_len(npair/2)) {
        ii <- 2 * i - 1
        D <- d[ii,]
        t <- c(t, ISOdatetime(1900+D$IY, D$IM, D$ID, 0:23, 0, 0, tz="UTC"))
        velo <- c(velo, D$KARD1, D$KARD2, D$KARD3, D$KARD4, D$KARD5,
                  D$KARD6, D$KARD7, D$KARD8, D$KARD9, D$KARD10,
                  D$KARD11, D$KARD12)
        D <- d[ii+1,]
        velo <- c(velo, D$KARD1, D$KARD2, D$KARD3, D$KARD4, D$KARD5,
                  D$KARD6, D$KARD7, D$KARD8, D$KARD9, D$KARD10,
                  D$KARD11, D$KARD12)
    }
    t <- numberAsPOSIXct(t)
    velo <- velo / 1000
    velo[velo > 50] <- NA
    list(time=t, velocityComponent=velo)
}
ns <- readComponent("tide8.dat")
ew <- readComponent("tide9.dat")
if (!all.equal(ns$time, ew$time))
    stop("times in north-south and east-west files do not match")
par(mfrow=c(2,1))
tidalCurrent <- data.frame(time=ns$time, u=ew$velocityComponent, v=ns$velocityComponent)
oce.plot.ts(tidalCurrent$time, tidalCurrent$u, ylab="u [m/s]")
oce.plot.ts(tidalCurrent$time, tidalCurrent$v, ylab="v [m/s]")
save(tidalCurrent, file="tidalCurrent.rda", version=2)
tools::resaveRdaFiles('tidalCurrent.rda', version=2)

