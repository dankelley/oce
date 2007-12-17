plot.ctd <- function (x, ref.lat = NaN, ref.lon = NaN, 
	grid = TRUE, col.grid="lightgray", ...) 
{
    dec_deg <- function(x, code = "lat")
	{
        if (code == "lat") {
            if (x < 0) {
                x <- -x
                sprintf("%.0f %.2fS", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("%.0f %.2fN", floor(x), 60 * (x - floor(x)))
            }
        }
        else {
            if (x < 0) {
                x <- -x
                sprintf("% %.2fW", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("% %.2fE", floor(x), 60 * (x - floor(x)))
            }
        }
    }
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    oldpar <- par(no.readonly = TRUE)
    par(mfrow = c(2, 2))
	par(mar=c(4,4,5,2))
    plot.profile(x, type = "S+T", grid=grid, col.grid=col.grid, ...)
	par(mar=c(5,4,3,2))
    plot.TS(x, grid=grid, col.grid=col.grid, ...)
	par(mar=c(4,4,5,2))
	plot.profile(x, type = "sigmatheta+N2", grid=grid, col.grid=col.grid, ...)
   	# Text
	text.item <- function(item, label, cex=1) {
		if (!is.null(item) && !is.na(item)) {
			text(xloc, yloc, paste(label, item), adj = c(0, 0), cex=cex);
    		yloc <<- yloc - d.yloc;
		}
	}
   	xfake <- seq(0:10)
    yfake <- seq(0:10)
	par(mar=c(0,0,0,0))
    plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
    xloc <- 1
    yloc <- 10
    d.yloc <- 0.8
	cex <- 0.9
    text(xloc, yloc, paste("CTD Station"), adj = c(0, 0), cex=cex)
    yloc <- yloc - d.yloc
	if (!is.null(x$filename))
    	text.item(x$filename,    " File:     ", cex=cex)
	if (!is.null(x$scientist))
		text.item(x$scientist,   " Scientist:", cex=cex);
	if (!is.null(x$institute))
		text.item(x$institute,   " Institute:", cex=cex);
	if (!is.null(x$date))
    	text.item(x$date,        " Date:     ", cex=cex)
	if (!is.null(x$ship))
		text.item(x$ship,	     " Ship:     ", cex=cex);
	if (!is.null(x$cruise))
    	text.item(x$cruise,      " Cruise:   ", cex=cex)
	if (!is.null(x$station))
    	text.item(x$station,     " Station:  ", cex=cex)
	if (!is.null(x$water.depth))
    	text.item(x$water.depth, " Depth:    ", cex=cex)
    if (!is.na(x$longitude) && !is.na(x$latitude)) { # See similar code in summary.ctd
    	text.item(latlon.format(x$latitude, x$longitude),         " Location: ", cex=cex)
	}
    if (!is.na(ref.lat) && !is.na(ref.lon)) {
        dist <- geod.dist(x$latitude, x$longitude, ref.lat, 
            ref.lon)
        kms <- sprintf("%.2f km", dist/1000)
        rlat <- text(xloc, yloc, paste(" Distance to (", dec_deg(ref.lon), 
            ",", dec_deg(ref.lat), ") = ", kms), adj = c(0, 0), cex=cex)
        yloc <- yloc - d.yloc
    }
	par(oldpar)
}
