library(oce)

read.xbt <- function(filename)
{
    l <- readLines(filename, 200) # don't read whole file
    ## FIXME: is this detection of the end of the header robust?
    headerEnd <- grep("^Depth \\(", l)
    ## FIXME: will data always be in this order?
    ## FIXME: is the data list always the same?
    data <- read.table(filename, skip=headerEnd+1, col.names=c("depth", "temperature", "soundspeed"))
    res <- new("oce")
    res@data <- data
    res@metadata$filename <- filename
    ## FIXME: read other metadata (location, time, etc.) once we know the format for sure
    res@metadata$units$depth <- list(unit=expression(m), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundspeed <- list(unit=expression(m/s), scale="")
    res
}
d <- read.xbt("fake.edf")
summary(d)
## FIXME: once we make a formal object, we can code plot methods
plot(d[["temperature"]], d[["depth"]], ylim=rev(range(d[["depth"]])))
## FIXME: get larger dataset (open-data) so we can work on subset() etc

