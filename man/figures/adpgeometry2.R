# png("adpgeometry2.png", width=500, height=300, pointsize=14)
png("adpgeometry2.png", width=400, height=300*400/500, pointsize=14)
sensorLocation <- c(0,0)
beamAngle <- 20
beamWidth <- 2.5
bin1Distance <- 2
binSize <- 1
nbin <- 4
distance <- seq(bin1Distance, by=binSize, length.out=nbin)
## assume distance is in vertical direction
y <- distance
yy <- c(0, distance, 1.1*distance)
## construct x so that atan2(y,x) is 90-beamAngle
x <- y * tan(beamAngle*pi/180)
par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
plot(x, y, asp=1, pch=1,
     xlim=max(x)*c(-1,1), ylim=c(0, 1.04*max(y)), yaxs="i",
     xlab="Horizontal coordinate [m]", ylab="Height above sensor [m]")
lines(yy * tan((beamAngle - beamWidth/2)*pi/180), yy)
lines(yy * tan((beamAngle + beamWidth/2)*pi/180), yy)
lines(yy * tan((-beamAngle - beamWidth/2)*pi/180), yy)
lines(yy * tan((-beamAngle + beamWidth/2)*pi/180), yy)
grid()
abline(v=0, lty="dashed")
points(sensorLocation, sensorLocation, pch=17, cex=3) # 
points(-x, y, pch=1)
dev.off()

