set.seed(1333334L)
library(oce)
## L <- read.lisst("~/Dropbox/LISST/WillapaBay_LISSTB.asc", 2010)
t <- seq(0, 6, 1/15) * 3600 + as.POSIXct("2012-01-01 00:00:00", tz="UTC")
n <- length(t)
p <- 5 + sin(as.numeric(t - t[1]) / 12.4 / 3600 * 2 * pi) + rnorm(n, sd=0.01)
dpdt <- c(0, diff(p))
T <- 10 + 5 * sin(as.numeric(t - t[1]) / 24 / 3600 * 2 * pi) + cumsum(rnorm(n, sd=0.2))
C <- (dpdt + rnorm(n, sd=0.1) + cumsum(rnorm(n, sd=0.5)))^2 * 2
sd <- rep(1, length.out=32) + (1:32) / 100
data <- matrix(nrow=n, ncol=42)
for (i in 1:32) {
    fake <- abs(C * (1 + i / 5) + cumsum(rnorm(n, sd=sd[i]))) / 100
    data[,i] <- fake
}
data[,33] <- rep(0, n) # lts
data[,34] <- rep(4, n) + 0.01 * cumsum(rnorm(n, 0.05)) # voltage
data[,35] <- rep(0.07, n) # aux
data[,36] <- runif(n, 3.9, 4.1) # lrs
data[,37] <- p
data[,38] <- T
tt <- as.POSIXlt(t)
data[,39] <- 100 * tt$yday + tt$hour
data[,40] <- 100 * tt$min + tt$sec
data[,41] <- abs((p - min(p)) / diff(range(p)) + cumsum(rnorm(n, sd=0.05))) # transmission
data[,42] <- 40 - 20*data[,41] # beam

#oce.plot.ts(t, p)
#oce.plot.ts(t, T)
#oce.plot.ts(t, C)
lisst <- as.lisst(data, filename="(constructed)", year=2012, "UTC")
#plot(lisst, which="voltage")
#plot(lisst, which=c(1, 10, 30))
#plot(lisst)
summary(lisst)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("lisst")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(lisst, file="lisst.rda", version=2)
    tools::resaveRdaFiles("lisst.rda", version=2)
} else {
    save(lisst, file="lisst.rda")
    tools::resaveRdaFiles("lisst.rda")
}

