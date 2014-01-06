library(oce)
# Plot TS diagram for data along 36N Atlantic section,
# colour-coded for longitude (which ranges -8 to -74)
data(section)
for (i in 1:124) {
    profile <- section[["station", i]]
    x <- (-8 - profile[["longitude"]]) / (74 - 8)
    col <- hsv(2*x/3) # red to blue
    if (i == 1) {
        plotTS(profile, Slim=c(34,37), Tlim=c(2,25), col=col)
    } else {
        points(profile[["salinity"]], profile[["temperature"]],pch=20,col=col)
    }
}
legend("bottomright", pch=rep(20,2), col=c("blue","red"),
    legend=c("West","East"), bg="white")
