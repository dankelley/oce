# oce object diagram (a terribly brittle kludge)
png("ctd-object-new.png", width=5, height=2.5, unit="in", res=150, pointsize=9)

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
}

par(mar=rep(0, 4))
plot(c(0,1.03), c(0,0.9), type="n", xaxs="i")
l <- 2.2 * strheight("x")              # vert offset
o <- 0.5 * l                           # horiz offset
xl <- 0.1
xm <- 0.5
xr <- 0.9
lcol <- "darkgray"                     # line colour

# Left
textInBox(xm, 0.92, "ctd object")
lines(rep(xm,2), c(0.83, 0.745), col=lcol)
lines(c(xl, xr), rep(0.79, 2), col=lcol)
lines(rep(xl, 2), c(0.79, 0.75), col=lcol)
lines(rep(xr, 2), c(0.79, 0.75), col=lcol)
textInBox(xl, 0.9-2*l, "       data       ")
textInBox(xl+o, 0.9-4*l, "salinity", 2.5)
textInBox(xl++o, 0.9-6*l, "temperature", 4.9)
textInBox(xl+o, 0.9-8*l, "pressure", 3)
textInBox(xl+o, 0.9-10*l, " ... ", 1.1)
lines(rep(xl-0.05, 2), c(0.66, 0.09), col=lcol)
lines(c(xl-0.05, xl-0.05+0.06), rep(0.55, 2), col=lcol)
lines(c(xl-0.05, xl-0.05+0.065), rep(0.4, 2), col=lcol)
lines(c(xl-0.05, xl-0.05+0.065), rep(0.24, 2), col=lcol)
lines(c(xl-0.05, xl-0.05+0.065), rep(0.09, 2), col=lcol)

# Middle
textInBox(xm, 0.9-2*l, "   metadata   ")
textInBox(xm+o, 0.9-4*l, "header", 2.11)
textInBox(xm+o, 0.9-6*l, "filename", 2.7)
textInBox(xm+o, 0.9-8*l, " ... ", 0.5)
lines(rep(xm-0.05, 2), c(0.66, 0.25), col=lcol)
lines(c(xm-0.05, xm-0.05+0.06), rep(0.55, 2), col=lcol)
lines(c(xm-0.05, xm-0.05+0.06), rep(0.40, 2), col=lcol)
lines(c(xm-0.05, xm-0.05+0.06), rep(0.25, 2), col=lcol)

# Right
textInBox(xr, 0.9-2*l, "processingLog")
textInBox(xr+o, 0.9-4*l, "time", 1)
textInBox(xr+o, 0.9-6*l, "action", 2)
lines(rep(xr-0.05, 2), c(0.66, 0.40), col=lcol)
lines(c(xr-0.05, xr-0.05+0.06), rep(0.55, 2), col=lcol)
lines(c(xr-0.05, xr-0.05+0.06), rep(0.40, 2), col=lcol)

dev.off()

