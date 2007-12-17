as.ctd <- function(S, T, p,
	header=NULL,
	filename=NA,ship=NA,scientist=NA,institute=NA,address=NA,
	cruise=NA,station=NA,date=NA,start.time=NA,
	latitude=NA, longitude=NA,
	recovery=NA,
	water.depth=NA,
	sample.interval=NA)
{
	if (length(p) == 1) # special case
		p = rep(p, length(S))
	data <- data.frame(salinity=S, temperature=T, pressure=p, sigma.theta=sw.sigma.theta(S, T, p))
	metadata <- list(
		header=header,
	    filename=filename,
		filename.orig=NULL,
		system.upload.time=NULL,
        ship=ship,
        scientist=scientist,
        institute=institute,
        address=address,
        cruise=cruise,
        station=station,
        date=date,
  		start.time=start.time,
       	latitude=latitude,
       	longitude=longitude,
       	recovery=recovery,
       	water.depth=water.depth,
       	sample.interval=sample.interval)
	log <- list(time=c(Sys.time()), action=c("created by as.ctd()"))
	res <- list(data=data, metadata=metadata, log=log)
  	class(res) <- "ctd"
	res
}
