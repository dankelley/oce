# towards-sensor view of 4-beam adcp (left) and 3-beam adv (right)
#pdf("ad-coords.pdf", width=7, height=3.3, pointsize=14)
png("adp_beams.png", width=3.5, height=1.6, unit="in", res=120, pointsize=9)
par(mar=c(0,0,0,0))
lwd.beam <- 1.75
lwd.axis <- 2
lwd.compass <- 1
cex.axis <- 1
cex.compass <- 3/4
f <- 0.8                                # axis label location
fbeam <- 0.6                            # beam label location
length.arrows <- 0.1

circle <- function(x0=0, y0=0, R=1, offset=0.2) {
    n <- 100                                # segments in circle
    xc <- x0 + R * cos(seq(0, 2*pi, length.out=n))
    yc <- y0 + R * sin(seq(0, 2*pi, length.out=n))
    lines(xc, yc, lwd=lwd.compass)
    text(x0, y0 - R, "S", cex=cex.compass, pos=1, offset=offset)
    text(x0 - R, y0, "W", cex=cex.compass, pos=2, offset=offset)
    text(x0, y0 + R, "N", cex=cex.compass, pos=3, offset=offset)
    text(x0 + R, y0, "E", cex=cex.compass, pos=4, offset=offset)
}
beam <- function(name, x, y, r=0.125, beam.dy=0.17)
{
    n <- 100                                # segments in circle
    xc <- x + r * cos(seq(0, 2*pi, length.out=n))
    yc <- y + r * sin(seq(0, 2*pi, length.out=n))
    polygon(xc, yc, lwd=lwd.beam, col="white")
    text(x, y+beam.dy, name, pos=1)
}

plot.default(c(-2.5,2.5), c(-1.0,1.0), asp=1, type='n') # adjust to tighten

## RDI (upward-looking orientation, viewed from above)
x.rdi <- -1.4
circle(x.rdi, 0)
arrows(x.rdi,     0, -1+x.rdi, 0, lwd=lwd.axis, length=length.arrows) # x
arrows(x.rdi,     0,    x.rdi, 1, lwd=lwd.axis, length=length.arrows) # y
text(x.rdi - f, 0, "x", pos=3, cex=cex.axis)
text(x.rdi, f, "y", pos=2, cex=cex.axis)
beam("3",          x.rdi,  fbeam)
beam("2", -fbeam + x.rdi,      0)
beam("1",  fbeam + x.rdi,      0)
beam("4",          x.rdi, -fbeam)

## Nortek and Sontek
x.nortek <- -x.rdi
circle(x.nortek, 0)
arrows(x.nortek, 0,   x.nortek, 1, lwd=lwd.axis, length=length.arrows) # x
arrows(x.nortek, 0, -1+x.nortek, 0, lwd=lwd.axis, length=length.arrows) # y
text(     x.nortek, f, "x", pos=2, cex=cex.axis)
text(-f + x.nortek, 0, "y", pos=3, cex=cex.axis)
beam("1",  x.nortek,      fbeam)
beam("2",  x.nortek + fbeam*sin(2*pi/3), fbeam*cos(2*pi/3))
beam("3",  x.nortek + fbeam*sin(4*pi/3), fbeam*cos(4*pi/3))

dev.off()
