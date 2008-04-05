read.etopo2 <- function(filename)
{
    nh <- 6
    header <- readLines(filename, n=nh)
    ncols <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
    nrows <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
    lon.center <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
    lat.center <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
    cellsize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
    zz <- as.matrix(read.table(filename, header=FALSE, skip=nh),byrow=TRUE)
    z <- t(zz[dim(zz)[1]:1,])
    lon <- lon.center + cellsize * seq(-ncols/2, ncols/2, length.out=ncols)
    lat <- lat.center + cellsize * seq(-nrows/2, nrows/2, length.out=nrows)
    data <- list(lon=lon, lat=lat, z=z)
    metadata <- list(filename=filename)
    log.item <- list(time=c(Sys.time()), action=c(paste("created by read.etopo2(\"",filename,"\")",sep="")))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("etopo2", "oce")
    res
}
