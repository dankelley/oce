library(oce)
message("Manual steps:")
message(" 1. visit http://www.naturalearthdata.com/downloads/110m-cultural-vectors")
message(" 2. click the link named 'Download countries' in the section called 'Admin 0 - Countries")
message(" 3. run this R file")

##> file <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip"
##> temp <- tempfile()
##> download.file(file, temp)
##> filename <- unz(temp, "ne_110m_admin_0_countries.shp")
### above not much good because cannot seek in this temp file.

coastlineWorld <- read.oce("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
save(coastlineWorld, file="coastlineWorld.rda")
tools::resaveRdaFiles("coastlineWorld.rda", compress="auto")
