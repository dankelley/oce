## m05_vector.R
library(oce)
m05VectorBeam <- read.oce("~/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",
    from=as.POSIXct("2008-06-25 00:00:00", tz="UTC"), 
    to=as.POSIXct("2008-07-06 00:00:00", tz="UTC"),
    latitude=47.87943, longitude=-69.72533)
m05VectorBeam <- retime(m05VectorBeam, a=0.58, b=6.3892e-07, t0=as.POSIXct("2008-07-01 00:00:00",tz="UTC"))
m05VectorBeam <- subset(m05VectorBeam, as.POSIXct("2008-06-25 13:00:00", tz="UTC") <= time &
    # time <= as.POSIXct("2008-07-04 17:00:00", tz="UTC")) # [sandbox/dk/022/05.R]
    time <= as.POSIXct("2008-07-03 00:50:00", tz="UTC")) # [sandbox/cr/011/01.R] based on voltage levels
m05VectorBeam <- oceEdit(m05VectorBeam, "transformationMatrix",
    rbind(c(11033, -5803, -5238), c(347, -9622, 9338), c(-1418, -1476, -1333)) / 4096,
    person="DEK", reason="Nortek email 2011-02-14")
# Take heading from aquadopphr
load("~/data/archive/sleiwex/2008/moorings/m05/adp/nortek_aqd2843/r/m05_aquadopphr_beam.rda")
aqdt <- as.numeric(m05AquadopphrBeam@data$time) # 10 s interval
vt <- as.numeric(m05VectorBeam@data$timeSlow) # 1s interval (varies a little)
print(sort(names(m05VectorBeam@metadata)))
print(sort(names(m05VectorBeam@data)))
#heading(m05VectorBeam) <- approx(aqdt-aqdt[1], m05AquadopphrBeam[["heading"]], vt-aqdt[1], rule=2)$y
diff(range(m05AquadopphrBeam@data$heading))
# Actually, m05AquadopphrBeam has constant heading (equal to -23.39001)
m05VectorBeam@data$headingSlow <- approx(aqdt-aqdt[1], m05AquadopphrBeam[["heading"]], vt-aqdt[1], rule=2)$y
#pitch(m05VectorBeam) <- despike(roll(m05VectorBeam))
m05VectorBeam@data$pitchSlow <- despike(m05VectorBeam@data$pitchSlow)
#roll(m05VectorBeam) <- despike(roll(m05VectorBeam))
m05VectorBeam@data$rollSlow <- despike(m05VectorBeam@data$rollSlow)

if (!interactive()) pdf("m05_vector_angles.pdf")
par(mfrow=c(3, 1))
oce.plot.ts(m05VectorBeam@data$timeSlow, m05VectorBeam@data$headingSlow)
oce.plot.ts(m05VectorBeam@data$timeSlow, m05VectorBeam@data$pitchSlow)
oce.plot.ts(m05VectorBeam@data$timeSlow, m05VectorBeam@data$rollSlow)
if (!interactive()) dev.off()

processingLog(m05VectorBeam) <- "use aquadoppHR heading; despike own pitch and roll"
#save(m05VectorBeam, file="/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_beam.rda")
save(m05VectorBeam, file="m05_vector_beam.rda")

m05VectorXyz <- beamToXyz(m05VectorBeam)
#rm(m05VectorBeam)
#save(m05VectorXyz, file="/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_xyz.rda")
save(m05VectorXyz, file="m05_vector_xyz.rda")
m05VectorEnu <- xyzToEnu(m05VectorXyz, declination=(-18.099),
                         cabled=TRUE, horizontalCase=TRUE, sensorOrientation="upward")
#rm(m05VectorXyz)
save(m05VectorEnu,  file="m05_vector_enu.rda")
new <- m05VectorEnu

cat("next is newly-read:\n")
print(new@data$v[1:10,])


