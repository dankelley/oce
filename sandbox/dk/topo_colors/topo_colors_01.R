rm(list=ls())
library(oce)
cex <- 4

## The previous colours (landOLD and waterOLD)
if (file.exists("old.rda")) {
    message("Getting landOLD and waterOLD from old.rda")
    load("old.rda")
} else {
    message("Saving landOLD and waterOLD in old.rda")
    landOLD <- oceColorsGebco(region="land", n=9)
    waterOLD <- oceColorsGebco(region="water", n=9)
    save(landOLD, waterOLD, file="old.rda")
}

if (!interactive())
    png("topo_colors_01.png", height=1.5, width=7, unit="in", res=150, pointsize=10)
par(mar=c(2.5,2.5,1,1), mgp=c(2,0.7,0))
#n <- 9
#landCol <- oceColorsGebco(region="land", n=n)
wlOLD <- c(waterOLD, landOLD)
plot(seq_along(wlOLD), rep(0, length.out=length(wlOLD)),
     pch=21, cex=cex, bg=wlOLD,
     xlim=c(0,20),
     ylim=c(-1,2),
     xlab="", ylab="")
abline(v=mean(seq_along(wlOLD)))

veryLightWater <- colorRampPalette(c(waterOLD[9], "#FFFFFF"), space="Lab")(4)[3]
lightLand <- colorRampPalette(c(landOLD[1], "#FFFFFF"), space="Lab")(3)[2]
veryLightLand <- colorRampPalette(c(lightLand, "#FFFFFF"), space="Lab")(5)[3]
##landNEW <- colorRampPalette(c(veryLightLand, lightLand, head(landOLD,8)), space="Lab", interpolate="spline")(10)
landNEW <- colorRampPalette(c(veryLightLand, lightLand, head(landOLD,8)))(10)
waterNEW <- c(waterOLD, veryLightWater)
wlNEW <- c(waterNEW, landNEW)
points(seq_along(wlNEW)-1, rep(1, length.out=length(wlNEW)),
       pch=21, cex=cex, bg=c(wlNEW))
mtext("Lower dots are existing scheme; upper dots are proposed scheme")

cat("Next are proposed water and land colours for oceColorsGebco() code:\n")
dput(waterNEW)
dput(landNEW)

check <- colorRampPalette(c(veryLightWater, veryLightLand))(3)
points(length(wlNEW)/2+seq(-1.5,0.5), rep(1.2, 3), bg=check, pch=21, cex=cex)

## Lengthen the water and land vectors, so we can add white paint in the middle
## without taking up 5% of the scale.

landNEWER <- colorRampPalette(landNEW)(20)
waterNEWER <- colorRampPalette(waterNEW)(20)
dput(landNEWER)
dput(waterNEWER)

if (!interactive())
    dev.off()
