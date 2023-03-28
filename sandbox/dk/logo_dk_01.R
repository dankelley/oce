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
data(endeavour, package="ocedata")
data(coastlineWorld, package="oce")
data(levitus, package="ocedata")
colBorder <- "#1f7bc2"
colBackground <- "#81b9e3"
colName <- "#075087"

oceLogo <- function()
{
    par(mar=rep(0.5, 4))
    mapPlot(coastlineWorld, col="tan", drawBox=FALSE, projection="+proj=natearth", border="black")
    Slim <- quantile(levitus$SSS, c(0.025, 1-0.025), na.rm=TRUE)
    # NOTE: filling in a few values near Antarctica, using
    # a hand-matched colour. Otherwise the eye can be drawn to
    # grey, or whatever missing-colour is used.
    cm <- colormap(levitus$SSS,
        breaks=seq(31, 37.5, 0.1),
        col=cmocean("haline"), missingColor=rgb(0, 128, 64, max=255))
    mapImage(levitus$longitude, levitus$latitude, levitus$SSS, colormap=cm)
    mapPolygon(coastlineWorld, col="tan", lwd=0.5)
    mapGrid(45, 45, col=gray(0.4), lwd=0.6)
    #mapLines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]],
    #         col="#916a13", lwd=0.5)
    mapPoints(endeavour$longitude, endeavour$latitude, pch=20, cex=0.5, lwd=0.2, col=2)
    text(58000, 13500000, "oce", cex=7.0, col=colName, font=2)
    text(58000,-14500000, "R", cex=7.5, col=colName, font=2)
}
sticker(~oceLogo(), package="",
        s_x=1.0,
        s_y=1.0,
        s_width=1.8,
        s_height=1.8,
        h_fill=colBackground,
        h_color=colBorder,
        p_color=colBackground,
        filename="logo_dk_01.png")
