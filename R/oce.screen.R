# Based on file src/library/graphics/R/screen.R
# with two modifications: the "mfg" line is commented out (see below)
# and a lot of things are renamed, to avoid name clashes.
# The license of this, as the original screen software and the OCE package, is GPL.
.oceSSenv <- new.env()

.oceSSget <- function(x) get(paste(x, dev.cur(), sep=":"), envir=.oceSSenv, inherits=FALSE)
.oceSSexists <- function(x) exists(paste(x, dev.cur(), sep=":"), envir=.oceSSenv, inherits=FALSE)
.oceSSassign <- function(x, value) assign(paste(x, dev.cur(), sep=":"), value, envir=.oceSSenv)
assign("par.list",
       c("xlog","ylog",
         "adj", "bty", "cex", "col", "crt", "err", "font", "lab",
         "las", "lty", "lwd", "mar", "mex",
         "fin", ## added
         ## "mfg", ## deleted
         "mgp", "pch",
         "pty", "smo", "srt", "tck", "usr",
         "xaxp", "xaxs", "xaxt", "xpd",
         "yaxp", "yaxs", "yaxt", "fig"), envir=.oceSSenv)

oce.split.screen <- function(figs, screen, erase = TRUE)
{
    first.split <- !.oceSSexists("oce.sp.screens")

    ##cat("in oce.split.screen(), first.split=", first.split, "\n")
    ##print(figs)

    if(missing(screen))
        screen <- if(!first.split) .oceSSget("oce.sp.cur.screen") else 0
    if(!first.split) .oce.valid.screens <- .oceSSget("oce.sp.valid.screens")
    if (missing(figs))
	if (first.split)
	    return(FALSE)
	else
	    return(.oce.valid.screens)
    if ((first.split && screen != 0) ||
	(!first.split && !(screen %in% .oce.valid.screens)))
	stop("invalid screen number ", screen)
    ## if figs isn't a matrix, make it one
    if (!is.matrix(figs)) {
	if (!is.vector(figs))
	    stop("'figs' must be a vector or a matrix with 4 columns")
	nr <- figs[1]
	nc <- figs[2]
	x <- seq.int(0, 1, length.out=nc+1)
	y <- seq.int(1, 0, length.out=nr+1)
	figs <- matrix(c(rep.int(x[-(nc+1)], nr), rep.int(x[-1], nr),
			 rep.int(y[-1], rep.int(nc, nr)),
			 rep.int(y[-(nr+1)], rep.int(nc, nr))),
		       ncol = 4)
    }
    num.screens <- nrow(figs)

    if (num.screens < 1)
	stop("'figs' must specify at least one screen")
    oce.new.screens <- oce.valid.screens <- cur.screen <- 0
    if (first.split) {
        if (erase) plot.new()
	## save the current graphics state
	split.saved.pars <- par(get("par.list", envir=.oceSSenv))
	split.saved.pars$fig <- NULL
	## NOTE: remove all margins when split screens
	split.saved.pars$omi <- par(omi=rep.int(0,4))$omi
	.oceSSassign("oce.sp.saved.pars", split.saved.pars)
	## set up the screen information
	oce.split.screens <- vector(mode="list", length=num.screens)
	oce.new.screens <- 1:num.screens
	for (i in oce.new.screens) {
	    oce.split.screens[[i]] <- par(get("par.list", envir=.oceSSenv))
	    oce.split.screens[[i]]$fig <- figs[i,]
	}
	oce.valid.screens <- oce.new.screens
	cur.screen <- 1
    } else {
        if (erase) erase.screen(screen)
	max.screen <- max(.oce.valid.screens)
	new.max.screen <- max.screen + num.screens
	oce.split.screens <- .oceSSget("oce.sp.screens")
	## convert figs to portions of the specified screen
	total <- c(0,1,0,1)
	if (screen > 0)
	    total <- oce.split.screens[[screen]]$fig
	for (i in 1:num.screens)
	    figs[i,] <- total[c(1,1,3,3)] +
		figs[i,]*rep.int(c(total[2]-total[1],
                                   total[4]-total[3]),
                                 c(2,2))
	oce.new.screens <- (max.screen+1):new.max.screen
	for (i in oce.new.screens) {
	    oce.split.screens[[i]] <- par(get("par.list", envir=.oceSSenv))
	    oce.split.screens[[i]]$fig <- figs[i-max.screen,]
	}
	oce.valid.screens <- c(.oce.valid.screens, oce.new.screens)
	cur.screen <- max.screen+1
    }
    .oceSSassign("oce.sp.screens", oce.split.screens)
    .oceSSassign("oce.sp.cur.screen", cur.screen)
    .oceSSassign("oce.sp.valid.screens", oce.valid.screens)
    if(first.split) on.exit(oce.close.screen(all.screens=TRUE))
    par(oce.split.screens[[cur.screen]])
    on.exit()
    return(oce.new.screens)
}

oce.screen <- function(n = cur.screen, new = TRUE)
{
    if (!.oceSSexists("oce.sp.screens"))
	return(FALSE)
    cur.screen <- .oceSSget("oce.sp.cur.screen")
    if (missing(n) && missing(new))
	return(cur.screen)
    if (!(n %in% .oceSSget("oce.sp.valid.screens")))
	stop("invalid screen number")
    oce.split.screens <- .oceSSget("oce.sp.screens")
    oce.split.screens[[cur.screen]] <- par(get("par.list", envir=.oceSSenv))
    .oceSSassign("oce.sp.screens", oce.split.screens)
    .oceSSassign("oce.sp.cur.screen", n)

    ##cat("in screen(n=",n,", new=",new,"), mar=",paste(oce.split.screens[[n]]$mar,collapse=" "), "and usr=", paste(oce.split.screens[[n]]$usr, collapse=" "), "\n")

    par(oce.split.screens[[n]])
    if (new)
	oce.erase.screen(n)
    invisible(n)
}

oce.erase.screen <- function(n = cur.screen)
{
    if (!.oceSSexists("oce.sp.screens"))
	return(FALSE)
    cur.screen <- .oceSSget("oce.sp.cur.screen")
    if (!(n %in% .oceSSget("oce.sp.valid.screens")) && n != 0)
	stop("invalid screen number")
    old <- par(usr=c(0,1,0,1), mar=c(0,0,0,0),
	       fig = if (n > 0)
	       .oceSSget("oce.sp.screens")[[n]]$fig
	       else
	       c(0,1,0,1),
	       xaxs="i", yaxs="i")
    on.exit(par(old))
    par(new=TRUE)
    plot.new()
    polygon(c(0,1,1,0), c(0,0,1,1), border=NA, col=0)
    par(new=TRUE)
    invisible()
}

oce.close.screen <- function(n, all.screens=FALSE)
{
    if (!.oceSSexists("oce.sp.screens"))
	return(FALSE)
    if (missing(n) && missing(all.screens))
	return(.oceSSget("oce.sp.valid.screens"))
    oce.valid.screens <- .oceSSget("oce.sp.valid.screens")
    if (all.screens || all(oce.valid.screens %in% n)) {
	par(.oceSSget("oce.sp.saved.pars") )
	par(mfrow=c(1,1), new=FALSE)
	rm(list=paste(c("oce.sp.screens", "oce.sp.cur.screen", "oce.sp.saved.pars",
           "oce.sp.valid.screens"), dev.cur(), sep=":"), envir=.oceSSenv)
	invisible()
    } else {
        oce.valid.screens <- oce.valid.screens[-sort(match(n, oce.valid.screens))]
	.oceSSassign("oce.sp.valid.screens", oce.valid.screens)
	temp <- .oceSSget("oce.sp.cur.screen")
	if (temp %in% n) {
            poss <- oce.valid.screens[oce.valid.screens>temp]
	    temp <- if(length(poss)) min(poss) else min(oce.valid.screens)
        }
	oce.screen(temp, new=FALSE)
	oce.valid.screens
    }
}
