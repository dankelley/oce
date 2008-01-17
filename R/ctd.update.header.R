ctd.update.header <- function (x, debug = FALSE)
{
	if (length(x$metadata$header) < 1)
		stop("there is no header in this CTD object")
	if (length(x$data) < 1)
		stop("there are no data in this CTD object")
	replace.header.element <- function(h, match, new)
	{
		for (i in 1:length(h)) {
			if (length(grep(match, h[i], perl=TRUE, useBytes=TRUE))) {
				h[i] <- new;
				break;
			}
		}
		return(h)
	}
                                        # adjust nvalues
                                        # ... fill in ...
                                        # adjust column ranges
	nquan <- length(x$data)
	xret <- x
	h <- xret$metadata$header
	for (i in 1:nquan) {
		r <- range(x$data[[i]])
		prefix <- sprintf("^#[\t ]*span[\t ]*%d[\t ]*=", i)
		span <- sprintf("# span %d = %g, %g", i, r[1], r[2]);
		h <- replace.header.element(h, prefix, span);
	}
	xret$header <- h
	return(xret)
}
