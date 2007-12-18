as.sealevel <- function(
	t=NULL,
	eta=NULL,
	header=NULL,
	station.number=NA,
	station.version=NA,
	station.name=NULL,
	region=NA,
	year=NA,
	latitude=NA,
	longitude=NA,
	GMT.offset=NA,
	decimation.method=NA,
	reference.offset=NA,
	reference.code=NA)
{    
	data <- data.frame(t=t, eta=eta)
	metadata <- list(header=header,
		year=year,
		station.number=station.number,
		station.version=station.version,
		station.name=station.name,
		region=region,
		latitude=latitude,
		longitude=longitude,
		GMT.offset=GMT.offset,
		decimation.method=decimation.method,
		reference.offset=reference.offset,
		reference.code=reference.code,
		units=units,
		n=length(eta))
	log.item <- list(time=c(Sys.time()), action=c("created by as.sealevel()"))
	rval <- list(data=data, metadata=metadata, processing.log=log.item)
	class(rval) <- "sealevel"
	rval
}
