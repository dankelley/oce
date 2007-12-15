section.subset <- function(section, indices=1:length(section$stations))
{
	n <- length(indices)
    stations <- vector("list", n)
    for (i in 1:n)
		stations[[i]] <- section$stations[[indices[i]]]
    processing.log <- section$processing.log
	section.id <- section$section.id
    res <- list(processing.log = processing.log, section.id = section.id, 
        stations = stations)
	log.item <- paste("modified by section.subset(x, indices=c(",paste(indices,collapse=","),"))",sep="")
	res <- processing.log.append(res, log.item)
    class(res) <- "section"
	res
}
