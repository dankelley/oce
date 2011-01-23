library(oce)
# Plot TS diagram for data along 36N Atlantic section,
# colour-coded for longitude (which ranges -8 to -74)
data(a03)
for (i in 1:124) {
    profile <- a03$data$station[[i]]
    x <- (-8 - profile$metadata$longitude) / (74 - 8)
    col <- hsv(2*x/3) # red to blue
    if (i == 1) {
        plot.TS(profile, Slim=c(34,37), Tlim=c(2,25), col=col)
    } else {
        points(profile$data$salinity, profile$data$temperature,pch=20,col=col)
    }
}
legend("bottomright", pch=rep(20,2), col=c("blue","red"),
    legend=c("West","East"), bg="white")
