read.topo <- function(filename, log.action)
{
    nh <- 6
    header <- readLines(filename, n=nh)
    ncols <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
    nrows <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
    lon.ll <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
    lat.ll <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
    cellsize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
    zz <- as.matrix(read.table(filename, header=FALSE, skip=nh),byrow=TRUE)
    z <- t(zz[dim(zz)[1]:1,])
    lon <- lon.ll + cellsize * seq(0, ncols-1)
    lat <- lat.ll + cellsize * seq(0, nrows-1)
    data <- list(lon=lon, lat=lat, z=z)
    metadata <- list(filename=filename, cellsize=cellsize, ncols=ncols, nrows=nrows, lon.ll=lon.ll, lat.ll=lat.ll)
    if (missing(log.action)) log.action <- paste(deparse(match.call(), sep="", collapse=""))
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("topo", "oce")
    res
}
