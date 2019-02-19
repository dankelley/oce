library(oce)
filename <- "xbt.edf"

read.xbt <- function(filename)
{
    getHeaderItem <- function(l, name) {
        res <- l[grep(name, l)]
        gsub(paste(name, "[ ]*:[ ]*", sep=""), "", res)
    }
    l <- readLines(filename, 200, encoding="UTF-8") # don't read whole file
    ## FIXME: is this detection of the end of the header robust?
    headerEnd <- grep("^Depth \\(", l)
    if (0 == length(headerEnd))
        stop("programming error: increase #lines read for header")
    res <- new("oce")
    date <- getHeaderItem(l, "Date of Launch")
    hms <- getHeaderItem(l, "Time of Launch")
    res@metadata$time <- as.POSIXct(paste(date, hms, sep=" "),
                                    format="%m/%d/%Y %H:%M:%S", tz="UTC")
    res@metadata$serialNumber <- getHeaderItem(l, "Serial #")
    res@metadata$sequenceNumber <- as.integer(getHeaderItem(l, "Sequence #"))
    ## Some steps needed for hemispheres.
    lat <- getHeaderItem(l, "Latitude")
    lats <- strsplit(lat, " ")[[1]]
    latdeg <- as.numeric(lats[1])
    latmin <- as.numeric(gsub("[NSns]", "", lats[2]))
    res@metadata$latitude <- (latdeg + latmin/60) * ifelse(length(grep("S", lats[2])), -1, 1)
    lon <- getHeaderItem(l, "Longitude")
    lons <- strsplit(lon, " ")[[1]]
    londeg <- as.numeric(lons[1])
    lonmin <- as.numeric(gsub("[EWew]", "", lons[2]))
    res@metadata$longitude <- (londeg + lonmin/60) * ifelse(length(grep("W", lons[2])), -1, 1)
    res@metadata$probeType <- getHeaderItem(l, "Probe Type")
    res@metadata$terminalDepth <- as.numeric(gsub("[ ]*m$", "", getHeaderItem(l, "Terminal Depth"))) # FIXME: assumes metric
    ## FIXME: will data always be in this order?
    ## FIXME: is the data list always the same?
    res@data <- read.table(filename, skip=headerEnd+1, col.names=c("depth", "temperature", "soundspeed"))
    res@metadata$filename <- filename
    res@metadata$units$depth <- list(unit=expression(m), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundspeed <- list(unit=expression(m/s), scale="")
    res
}
d <- read.xbt("xbt.edf")
summary(d)
## FIXME: once we make a formal object, we can code plot methods
if (!interactive()) png("1494a.png")
plot(d[["temperature"]], d[["depth"]], ylim=rev(range(d[["depth"]])), type="l")
## FIXME: get larger dataset (open-data) so we can work on subset() etc

cat("next is the metadata ... we'll get more display of this in summary() when this is formalized\n")
print(d@metadata)
if (!interactive()) dev.off()

