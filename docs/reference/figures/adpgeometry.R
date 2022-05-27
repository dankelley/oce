#png("adpgeometry.png", width=500, height=300, pointsize=14)
png("adpgeometry.png", width=400, height=300*400/500, pointsize=14)
sensorLocation <- c(0,0)
beamAngle <- 20
bin1Distance <- 2
binSize <- 1
nbin <- 4
distance <- seq(bin1Distance, by=binSize, length.out=nbin)
## assume distance is in vertical direction
y <- distance
## construct x so that atan2(y,x) is 90-beamAngle
x <- y * tan(beamAngle*pi/180)
par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
plot(x, y, asp=1, pch=1,
     xlim=max(x)*c(-1,1), ylim=c(0, 1.04*max(y)), yaxs="i",
     xlab="Horizontal coordinate [m]", ylab="Height above sensor [m]")
grid()
abline(v=0, lty="dashed")
points(sensorLocation, sensorLocation, pch=17, cex=3) # 
points(-x, y, pch=1)
abline(0, tan((90-beamAngle)*pi/180))
abline(0, tan((90+beamAngle)*pi/180))
dev.off()

