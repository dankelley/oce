library(oce)
data(coastlineWorld)
plot(coastlineWorld)
hfxLat <-  44+39/60
hfxLon <- -(63+34/60)
dx <- 5
dy <- 5
lines(hfxLon+c(dx,dx,-dx,-dx,dx), hfxLat+c(-dy,dy,dy,-dy,-dy), lwd=1, col="blue")
text(hfxLon+0.1, hfxLat-0.15, "Halifax", col = "blue", pos=4, cex=1.3)
