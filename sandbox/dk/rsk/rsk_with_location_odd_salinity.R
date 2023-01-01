# Use 'rsk' branch
library(oce)
source("~/git/oce/R/rsk.R")
source("~/git/oce/R/ctd.R")
f <- "~/ctd_with_location.rsk"
d <- read.rsk(f, allTables=TRUE, tzOffsetLocation=-8)

if (!interactive())
    png("rsk_with_location_odd_salinity_%d.png",
        width=7, height=7, unit="in", res=200)

ctds <- ctdFindProfilesRBR(as.ctd(d), debug=1)

par(mfrow=c(2,1), mgp=c(1.5,0.6,0), mar=c(2,2,1,1))

for (i in seq_along(ctds))
{
    plotScan(ctds[[i]])
    mtext(paste("Station ", ctds[[i]][["station"]]))
    plotScan(ctds[[i]], which=4)
}

plot(d[["time"]], d[["pressure"]], type="l")
plot(d[["time"]], d[["salinity"]], type="l")

