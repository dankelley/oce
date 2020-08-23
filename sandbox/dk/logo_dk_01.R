## Trial hex logo for oce.
##
## This is mainly a test of whether a map showing SSS and the Endeavour cruise
## would look interesting.  I think it does. Using SST is less interesting, I
## think, because SSS shows gyres better.  The colours, line widths, etc are
## provisional, but the fiddling with size and offset is probably close to
## final.

library(oce)
library(cmocean)
library(hexSticker)
colBorder <- "#1f7bc2"
colBackground <- "#81b9e3"
colName <- "#075087"

oceLogo <- function()
{
    par(mar=rep(0.5, 4))
    data(endeavour, package="ocedata")
    data(coastlineWorld, package="oce")
    mapPlot(coastlineWorld, col="tan", drawBox=FALSE, projection="+proj=natearth")
    data(levitus, package="ocedata")
    Slim <- quantile(levitus$SSS, c(0.025, 1-0.025), na.rm=TRUE)
    cm <- colormap(levitus$SSS,
                   breaks=seq(31, 37.5, 0.1),
                   col=cmocean("haline"), missingColor="tan")
    mapImage(levitus$longitude, levitus$latitude, levitus$SSS, colormap=cm)
    mapGrid(30, 30, col=gray(0.4), lwd=0.6)
    mapLines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]],
             col="#916a13", lwd=0.3)
    mapPoints(endeavour$longitude, endeavour$latitude, pch=20, cex=0.5, lwd=0.2, col=2)
    text(58000, 14500000, "oce", cex=2.0, col=colName, font=2)
}
sticker(~oceLogo(), package="",
        s_x=0.8,
        s_y=0.7,
        s_width=2.2,
        s_height=2.2,
        h_fill=colBackground,
        h_color=colBorder,
        p_color=colBackground,
        filename="logo_dk_01.png")
