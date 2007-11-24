ctd.write <- function(object, file=stop("'file' must be specified"))
{
  	if (!inherits(object, "ctd"))
    	stop("method is only for ctd objects")
	if (is.character(file)) {
		if (file == "")
			stop("'file' must be a non-empty string")
		con <- file(file, "w")
	}
	else if (inherits(file, "connection"))
		con <- file
	cat(object$header, sep="\n", file=con) # bug: ranges are wrong
	write.table(object$data, col.names=FALSE,row.names=FALSE, file=con)
	close(con)
}
 
