# oce object diagram (a terribly brittle kludge)
png("ctd-object.png", width=5, height=2.5, unit="in", res=150, pointsize=9)

textInBox <- function(x, y, text, xoff=0, yoff=0,
    col="black", cex=1, font=1, lwd=1, pos=1)
{
    w <- strwidth(text)
    h <- strheight(text)
    em <- strwidth("x")
    ex <- strheight("x")
    x <- x + xoff * em
    y <- y + yoff * ex
    text(x, y, text, col=col, cex=cex, font=font, pos=pos)
    xl <- x - w/2 - ex/2
    xr <- x + w/2 + ex/2
    yb <- y - 2*h - ex/2
    yt <- y
    lines(c(xl, xl, xr, xr, xl), c(yb, yt, yt, yb, yb), col=col, lwd=lwd)
    list(xl=xl, xr=xr, yb=yb, yt=yt, xm=0.5*(xl+xr), ym=0.5*(yb+yt))
}

par(mar=rep(0, 4))
plot(c(0,1.03), c(0,0.9), type="n", xaxs="i")
l <- 2.2 * strheight("x")              # vert offset
o <- 0.5 * l                           # horiz offset
xl <- 0.1
xm <- 0.5
xr <- 0.9
lcol <- "darkgray"                     # line colour
llwd <- 2

# Middle
ctdObject  <- textInBox(xm, 0.92, "ctd object")
# Left
data <- textInBox(xl, 0.9-2*l, "      data      ")
dataSalinity <- textInBox(xl+o, 0.9-4*l, "salinity", 2.5)
dataTemperature <- textInBox(xl++o, 0.9-6*l, "temperature", 4.9)
dataPressure <- textInBox(xl+o, 0.9-8*l, "pressure", 3.4)
dataDots <- textInBox(xl+o, 0.9-10*l, " ... ", 1.1)
# Middle
metadata <- textInBox(xm, 0.9-2*l, "   metadata   ")
metadataUnits <- textInBox(xm+o, 0.9-4*l, "units", 1.5)
metadataFlags <- textInBox(xm+o, 0.9-6*l, "flags", 1.5)
metadataDots <- textInBox(xm+o, 0.9-8*l, " ... ", 0.7)
# Right
processingLog <- textInBox(xr, 0.9-2*l, "processingLog")
processingLogTime <- textInBox(xr+o, 0.9-4*l, "time", 1)
processingLogAction <- textInBox(xr+o, 0.9-6*l, "action", 2)

# Join 'ctd object' with its three descendents
lines(rep(xm,2), c(ctdObject$yb, metadata$yt), col=lcol, lwd=llwd)
lines(rep(xm-0.04,2), c(ctdObject$yb, 0.5*(ctdObject$yb+metadata$yt)), col=lcol, lwd=llwd)
lines(c(data$xm, xm-0.04), rep(0.5*(ctdObject$yb+metadata$yt),2), col=lcol, lwd=llwd)
lines(c(processingLog$xm, xm+0.04), rep(0.5*(ctdObject$yb+metadata$yt),2), col=lcol, lwd=llwd)
lines(rep(xm+0.04,2), c(ctdObject$yb, 0.5*(ctdObject$yb+metadata$yt)), col=lcol, lwd=llwd)
#lines(rep(xm,2), c(ctdObject$yb, metadata$yt), col=lcol, lwd=llwd)
ymid <- 0.5 * (ctdObject$yb + metadata$yt)
#lines(c(xl, xr), rep(ymid, 2), col=lcol, lwd=llwd)
lines(rep(xl, 2), c(ymid, data$yt), col=lcol, lwd=llwd)
lines(rep(xr, 2), c(ymid, data$yt), col=lcol, lwd=llwd)
# Join 'data' and its 4 descendents
lines(rep(xl-0.05, 2), c(data$yb, with(dataDots, (yb+yt)/2)), col=lcol, lwd=llwd)
DX <- 0.05
lines(c(xl-DX, dataSalinity$xl), rep(with(dataSalinity, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xl-DX, dataSalinity$xl), rep(with(dataTemperature, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xl-DX, dataSalinity$xl), rep(with(dataPressure, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xl-DX, dataSalinity$xl), rep(with(dataDots, (yb+yt)/2), 2), col=lcol, lwd=llwd)
# Join 'metadata' and its 4 descendents
lines(rep(xm-DX, 2), c(metadata$yb, with(metadataDots, (yb+yt)/2)), col=lcol, lwd=llwd)
lines(c(xm-DX, metadataUnits$xl), rep(with(metadataUnits, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xm-DX, metadataUnits$xl), rep(with(metadataFlags, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xm-DX, metadataUnits$xl), rep(with(metadataDots, (yb+yt)/2), 2), col=lcol, lwd=llwd)
# Join 'processingLog' and its 2 descendents
lines(rep(xr-DX, 2), c(processingLog$yb, processingLogAction$ym), col=lcol, lwd=llwd)
lines(c(xr-DX, processingLogTime$xl), rep(with(processingLogTime, (yb+yt)/2), 2), col=lcol, lwd=llwd)
lines(c(xr-DX, processingLogTime$xl), rep(with(processingLogAction, (yb+yt)/2), 2), col=lcol, lwd=llwd)

dev.off()

