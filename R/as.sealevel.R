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
	reference.code=NA,
	processing.log=NULL)
{    
	if (is.null(processing.log))
		processing.log <- list(time=c(Sys.time()), action=c("created by as.sealevel()"))
	rval <- list(header=header,
		station.number=station.number,
		station.version=station.version,
		station.name=station.name,
		region=region,
		year=year,
		latitude=latitude,
		longitude=longitude,
		GMT.offset=GMT.offset,
		decimation.method=decimation.method,
		reference.offet=reference.offset,
		reference.code=reference.code,
		units=units,
		processing.log=processing.log,
		n=length(eta),
		data=list(t=t, eta=eta))
	class(rval) <- "sealevel"
	rval
}
