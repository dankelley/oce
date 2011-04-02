##subset(, as.POSIXct("2008-06-23 05:00:00", tz="UTC") <= time & time <= as.POSIXct("2008-07-04 20:00:00", tz="UTC"))
library(oce)
deployment <- function(into.water=as.POSIXct("2008-06-25 15:00:00", tz="UTC")) # pt1 shows 16:00:00
{
    cat("Deployment estimate:", format(into.water), "\n")
    plot(early, which='temperature', draw.time.range=FALSE)
    abline(v=into.water, col='red', lty='dotted')
    mtext(format(into.water), side=3, line=0, at=into.water, cex=3/4, col="red")
    oce.plot.ts(early$data$ts$time, as.numeric(early$data$ma$amp[,5,1]),draw.time.range=FALSE,main="m08 Sontek ADP amp[,5,1]")
    abline(v=into.water, col='red', lty='dotted')
    return(trunc(into.water, "hours") + 3600)
}
recovery <- function(out.of.water=as.POSIXct("2008-07-04 20:00:00", tz="UTC"))
{
    cat("Recovery estimate:", format(out.of.water), "\n")
    plot(late, which='temperature', draw.time.range=FALSE)
    abline(v=out.of.water, col='red', lty='dotted')
    mtext(format(out.of.water), side=3, line=0, at=out.of.water, cex=3/4, col="red")
    oce.plot.ts(late$data$ts$time, as.numeric(late$data$ma$amp[,5,1]),draw.time.range=FALSE,main="m08 Sontek ADP amp[,5,1]")
    abline(v=out.of.water, col='red', lty='dotted')
    return(trunc(out.of.water, "hours"))
}
if (0 == length(ls(pattern="^m08.adp.beam$"))) { # cache, for interactive use
    toc <- logger.toc("/data/archive/sleiwex/2008/moorings/m08/adp/sontek_c360/raw",
                      from=as.POSIXct("2008-06-01 00:00:00", tz="UTC"),
                      to=as.POSIXct("2008-07-30 00:00:00", tz="UTC"))
    m08.adp.beam <- read.adp.sontek.serial(file=toc$filename, latitude=47.88077, longitude=-69.72986, orientation="upward")
    m08.adp.beam <- retime(m08.adp.beam, a=93091, b=0, t0=as.POSIXct("2008-07-01 00:00:00", tz="UTC"))
}
early <- subset(m08.adp.beam, 
                as.POSIXct("2008-06-25 00:00:00",tz="UTC")<=time&time<=as.POSIXct("2008-06-25 20:00:00",tz="UTC"))
late <- subset(m08.adp.beam, 
                as.POSIXct("2008-07-04 10:00:00",tz="UTC")<=time&time<=as.POSIXct("2008-07-04 23:00:00",tz="UTC"))
if (!interactive())
    png("08adp.png", height=600, width=800, pointsize=14)
par(mfcol=c(2,2))
t.deployment <- deployment()
t.recovery <- recovery()
cat("\nUse the following for data:\n")
cat("subset(, as.POSIXct(\"", format(t.deployment), "\", tz=\"UTC\") <= time & time <= as.POSIXct(\"", format(t.recovery), "\", tz=\"UTC\"))\n", sep="")
