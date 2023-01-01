# based on code at
# https://github.com/dankelley/oce/issues/2026#issuecomment-1366950703
library(oce)
source("~/git/oce/R/ctd.R")

f <- "~/Dropbox/ctd_with_location.rsk"

#Read data.  The time offset is particular to this data file.
d <- read.rsk(f, tzOffsetLocation=-8)

#Chop into profiles, using information in RBR headers
ctds0 <- ctdFindProfilesRBR(as.ctd(d))

#Ignore top 2m and bottom 3m, which may be incorrect.
ctds1 <- lapply(ctds0,
                function(ctd) {
                  p <- ctd[["pressure"]]
                  subset(ctd, 2 < p & p < max(p) - 3)
                })

#create a section with a subset of the ctd casts
section_aialik = list(ctds1[[16]],ctds1[[17]],ctds1[[18]],
                      ctds1[[19]],ctds1[[20]],ctds1[[21]]) |>
  as.section()

if (!interactive())
    png("rsk_section_%d.png")

#make contour plot with distance on the x-axis
plot(section_aialik, xtype='distance', ztype='image')

# Expand plots to see a bit more (but there is very little distance here so the
# profiles are almost identical).  I'm using 'turbo' colours to try to see if
# colours are horizontal on the page, which is hard to tell with the
# usually-preferred perceptive colourschemes.
plot(section_aialik, which="temperature", xtype="distance", ztype="image",
    zcol=oceColorsTurbo)
plot(section_aialik, which="salinity", xtype="distance", ztype="image",
    zcol=oceColorsTurbo)

# A TS plot is handy.
plotTS(section_aialik)

if (!interactive())
    dev.off()
