library(oce)
data(coastline.maritimes)
plot(coastline.maritimes, col="darkred")
hfx.lat <-  44+39/60
hfx.lon <- -(63+34/60)
dx <- 0.175
dy <- 0.15
lines(hfx.lon+c(dx,dx,-dx,-dx,dx), hfx.lat+c(-dy,dy,dy,-dy,-dy), lwd=1, col="blue")
text(hfx.lon+0.1, hfx.lat-0.15, "Halifax", col = "blue", pos=4, cex=1.3)
