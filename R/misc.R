## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

T68fromT90 <- function(temperature) temperature * 1.00024
T90fromT68 <- function(temperature) temperature / 1.00024
T90fromT48 <- function(temperature) (temperature-4.4e-6*temperature*(100-temperature))/1.00024

#' Set any flagged data to NA
#'
#' If this is applied to a non-oce object, then that object is returned
#' unchanged. If it is applied to an object that lacks \code{metadata$flags},
#' then \code{x} is again returned unchanged, but a warning is issued.
#'
#' If \code{action} is \code{"NA"}, then any flagged data are set to \code{NA}.
#' Any other \code{action} yields an error.
#'
#' Note that this only works for objects of \code{\link{argo-class}}, so far.
#'
#' @param x An oce object.
#' @param action The action to be undertaken.
#' @return Either a non-oce object returned as-is, or an oce object that may have been modified to account for flags.
#' @examples
#' data(argo)
#' par(mfcol=c(2, 2))
#' plot(argo, which=2)
#' plot(handleFlagsOLD(argo), which=2)
#' plot(argo, which=3)
#' plot(handleFlagsOLD(argo), which=3)
handleFlagsOLD <- function(x, action="NA")
{
    if (!inherits(x, "oce"))
        return(x)
    if (!("flags" %in% names(x@metadata))) {
        warning("x does not contain an item named 'flags' in its metadata slot")
        return(x)
    }
    if ("NA" != action) stop("the only permitted action is \"NA\"")
    fnames <- names(x@metadata$flags)
    fnamesPlain <- gsub("Qc$","",fnames) # for argo ... defunct but no harm
    dnames <- names(x@data)
    for (name in fnamesPlain) {
        if (name %in% dnames) {
            if (inherits(x, "argo")) {
                bad <- x@metadata$flags[[name]] != "1"
                x@data[[name]][bad] <- NA
            } else {
                warning("cannot handle flags for an object of class \"", class(x)[1], "\"")
            }
        } else {
            warning("no item named \"", name, "\" in data")
        }
    }
    x
}

#' Calculate a rounded bound, rounded up to matissa 1, 2, or 5
#'
#' @param x a single positive number
#' @return for positive x, a value exceeding x that has mantissa 1, 2, or 5; otherwise, x
bound125 <- function(x)
{
    x <- x[1] # ignore all but first element
    if (x <= 0) {
        res <- x
    } else {
        exp10 <- 10^floor(log10(x))
        xx <- x / exp10
        m <- if (xx <= 1) 1 else if (xx <=2) 2 else if (xx <= 5) 5 else 10
        res <- m * exp10
        ##> r <- 10^rnorm(1e4)
        ##> R <- unlist(lapply(1:1e4, function(i) bound125(r[i]))
        ##> range(r/R)
        ##> message("x: ", x, ", exp10: ", exp10, ", m: ", m, ", xx: ", xx, ", res: ", res)
    }
    res
}

#' Put longitude in the range from -180 to 180
#'
#' @param longitude in degrees East, possibly exceeding 180
#' @return longitude in signed degrees East
#' @seealso
#' \code{\link{matrixShiftLongitude}} and \code{\link{shiftLongitude}} are more
#' powerful relatives to \code{standardizeLongitude}.
standardizeLongitude <- function(longitude) ifelse(longitude > 180, longitude-360, longitude)

#' Show an argument to a function, e.g. for debugging
#'
#' @param x the argument
#' @param nshow number of values to show at first (if length(x)> 1)
#' @param last indicates whether this is the final argument to the function
#' @param sep the separator between name and value
argShow <- function(x, nshow=2, last=FALSE, sep="=")
{
    if (missing(x))
        return("")
    name <- paste(substitute(x))
    res <- ""
    if (missing(x)) {
        res <- "(missing)"
    } else {
        if (is.null(x)) {
            res <- NULL
        } else {
            nx <- length(x)
            if (nx > 1)
                name <- paste(name, "[", nx, "]", sep="")
            if (is.function(x)) {
                res <- "(provided)"
            } else if (is.character(x) && nx==1) {
                res <- paste('"', x[1], '"', sep="")
            } else {
                look <- 1:min(nshow, nx)
                res <- paste(format(x[look], digits=4), collapse=" ")
                if (nx > nshow)
                    res <- paste(res, "...", x[nx])
            }
        }
    }
    if (!last)
        res <- paste(res, ", ", sep="")
    paste(name, res, sep="=")
}

#' Try to associate data names with units, for use by summary()
#'
#' Note that the whole object is not being given as an argument;
#' possibly this will reduce copying and thus storage impact.
#'
#' @param names the names of data within an object
#' @param units the units from metadata
#' @return a vector of strings, with blank entries for data with unknown units
#' @examples
#' library(oce)
#' data(ctd)
#' dataLabel(names(ctd@@data), ctd@@metadata$units)
dataLabel <- function(names, units)
{
    res <- names
    ##message("in dataLabel()")
    if (!missing(units)) {
        ## message("  dataLabel(); next line is names")
        ## print(names)
        ## message("  dataLabel(); next line is units")
        ## print(units)
        unitsNames <- names(units)
        for (i in seq_along(names)) {
            ##> message("  i: ", i, ", name: ", names[i])
            w <- which(unitsNames == names[i])
            if (length(w)) {
                ## message("  we match a unit at index w=",  w)
                u <- units[w]
                if (!is.null(u)) {
                    if (is.character(u)) {
                        res[i] <- paste(res[i], " [", u, "]", sep="")
                    } else if (is.list(u)) {
                        res[i] <- paste(res[i], " [", u$unit[[1]], u$scale, "]", sep="")
                    }
                }
            }
        }
    }
    ##> message("names:", paste(names, collapse=" | "))
    ##> message("units:", paste(units, collapse=" | "))
    ##> message("res:", paste(res, collapse=" | "))
    res <- gsub(" *\\[\\]", "", res)
    ##message("dataLabel() returning:")
    ##print(res)
    res
}

curl <- function(u, v, x, y, geographical=FALSE, method=1)
{
    if (missing(u)) stop("must supply u")
    if (missing(v)) stop("must supply v")
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (length(x) <= 1) stop("length(x) must exceed 1 but it is ", length(x))
    if (length(y) <= 1) stop("length(y) must exceed 1 but it is ", length(y))
    if (length(x) != nrow(u)) stop("length(x) must equal nrow(u)")
    if (length(y) != ncol(u)) stop("length(x) must equal ncol(u)")
    if (nrow(u) != nrow(v)) stop("nrow(u) and nrow(v) must match")
    if (ncol(u) != ncol(v)) stop("ncol(u) and ncol(v) must match")
    if (!is.logical(geographical)) stop("geographical must be a logical quantity")
    method <- as.integer(round(method))
    if (1 == method)
        res <- .Call("curl1", u, v, x, y, geographical)
    else if (2 == method)
        res <- .Call("curl2", u, v, x, y, geographical)
    else
        stop("method must be 1 or 2")
    res
}

rangeExtended <- function(x, extend=0.04) # extend by 4% on each end, like axes
{
    if (length(x) == 1) {
        x * c(1 - extend, 1 + extend) 
    } else {
        r <- range(x, na.rm=TRUE)
        d <- diff(r)
        c(r[1] - d * extend, r[2] + d * extend)
    }
}

binApply1D <- function(x, f, xbreaks, FUN, ...)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(f)) stop("must supply 'f'")
    if (missing(xbreaks)) xbreaks <- pretty(x, 20)
    if (missing(FUN)) stop("must supply 'FUN'")
    if (!is.function(FUN)) stop("'FUN' must be a function")
    ## FIXME: maybe employ the code below to get data from oce objects
    ##if ("data" %in% slotNames(x)) # oce objects have this
    ##    x <- x@data
    ##t <- try(x <- data.frame(x), silent=TRUE)
    ##if (class(t) == "try-error")
    ##    stop("cannot coerce 'data' into a data.frame")
    fSplit <- split(f, cut(x, xbreaks))
    res <- sapply(fSplit, FUN, ...)
    names(res) <- NULL
    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks), result=res)
}

binApply2D <- function(x, y, f, xbreaks, ybreaks, FUN, ...)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (missing(f)) stop("must supply 'f'")
    nx <- length(x)
    if (nx != length(y)) stop("lengths of x and y must agree")
    if (missing(xbreaks)) xbreaks <- pretty(x, 20)
    if (missing(ybreaks)) ybreaks <- pretty(y, 20)
    if (missing(FUN)) stop("must supply 'FUN'")
    if (!is.function(FUN)) stop("'FUN' must be a function")
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) stop("must have more than 1 ybreak")
    res <- matrix(nrow=nxbreaks-1, ncol=nybreaks-1)
    A <- split(f, cut(y, ybreaks))
    B <- split(x, cut(y, ybreaks))
    for (i in 1:length(A)) {
        fSplit <- split(A[[i]], cut(B[[i]], xbreaks))
        ##res[,i] <- binApply1D(B[[i]], A[[i]], xbreaks, FUN)$result
        res[,i] <- sapply(fSplit, FUN, ...)
    }
    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks), 
         ybreaks=ybreaks, ymids=ybreaks[-1]-0.5*diff(ybreaks),
         result=res)
}

binCount1D <- function(x, xbreaks)
{
    if (missing(x)) stop("must supply 'x'")
    ##nx <- length(x)
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 break")
    res <- .C("bin_count_1d", length(x), as.double(x),
              length(xbreaks), as.double(xbreaks),
              number=integer(nxbreaks-1),
              result=double(nxbreaks-1),
              NAOK=TRUE, PACKAGE="oce")
    list(xbreaks=xbreaks,
         xmids=xbreaks[-1]-0.5*diff(xbreaks), 
         number=res$number)
}

binMean1D <- function(x, f, xbreaks)
{
    if (missing(x)) stop("must supply 'x'")
    fGiven <- !missing(f)
    if (!fGiven)
        f <- rep(1, length(x))
    nx <- length(x)
    if (nx != length(f))
        stop("lengths of x and f must agree")
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 break")
    res <- .C("bin_mean_1d", length(x), as.double(x), as.double(f),
              length(xbreaks), as.double(xbreaks),
              number=integer(nxbreaks-1),
              result=double(nxbreaks-1),
              NAOK=TRUE, PACKAGE="oce")
    list(xbreaks=xbreaks,
         xmids=xbreaks[-1]-0.5*diff(xbreaks), 
         number=res$number,
         result=if (fGiven) res$result else rep(NA, length=nx))
}

##binWhich1D <- function(x, xbreaks)
##{
##    if (missing(x)) stop("must supply 'x'")
##    if (missing(xbreaks))
##        xbreaks <- pretty(x)
##    nxbreaks <- length(xbreaks)
##    if (nxbreaks < 2)
##        stop("must have more than 1 break")
##    res <- .C("bin_which_1d", length(x), as.double(x),
##               length(xbreaks), as.double(xbreaks),
##               bi=integer(length(x)),
##               NAOK=TRUE, PACKAGE="oce")
##    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks), bi=res$bi)
##}


binCount2D <- function(x, y, xbreaks, ybreaks, flatten=FALSE)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (length(x) != length(y)) stop("lengths of x and y must agree")
    if (missing(xbreaks)) xbreaks <- pretty(x)
    if (missing(ybreaks)) ybreaks <- pretty(y)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) stop("must have more than 1 ybreak")
    M <- .C("bin_count_2d", length(x), as.double(x), as.double(y),
            length(xbreaks), as.double(xbreaks),
            length(ybreaks), as.double(ybreaks),
            number=integer((nxbreaks-1)*(nybreaks-1)),
            mean=double((nxbreaks-1)*(nybreaks-1)),
            NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
                 ybreaks=ybreaks,
                 xmids=xbreaks[-1]-0.5*diff(xbreaks),
                 ymids=ybreaks[-1]-0.5*diff(ybreaks),
                 number=matrix(M$number, nrow=nxbreaks-1))
    if (flatten) {
        res2 <- list()
        res2$x <- rep(res$xmids, times=nybreaks-1)
        res2$y <- rep(res$ymids, each=nxbreaks-1)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    res
}


binMean2D <- function(x, y, f, xbreaks, ybreaks, flatten=FALSE, fill=FALSE)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    fGiven <- !missing(f)
    if (!fGiven)
        f <- rep(1, length(x))
    if (length(x) != length(y)) stop("lengths of x and y must agree")
    if (length(x) != length(f)) stop("lengths of x and f must agree")
    if (missing(xbreaks)) xbreaks <- pretty(x)
    if (missing(ybreaks)) ybreaks <- pretty(y)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) stop("must have more than 1 ybreak")
    M <- .C("bin_mean_2d", length(x), as.double(x), as.double(y), as.double(f),
            length(xbreaks), as.double(xbreaks),
            length(ybreaks), as.double(ybreaks),
            as.integer(fill), 
            number=integer((nxbreaks-1)*(nybreaks-1)),
            mean=double((nxbreaks-1)*(nybreaks-1)),
            NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
                 ybreaks=ybreaks,
                 xmids=xbreaks[-1]-0.5*diff(xbreaks),
                 ymids=ybreaks[-1]-0.5*diff(ybreaks),
                 number=matrix(M$number, nrow=nxbreaks-1),
                 result=if (fGiven) matrix(M$mean, nrow=nxbreaks-1) else matrix(NA, ncol=nybreaks-1, nrow=nxbreaks-1))
    if (flatten) {
        res2 <- list()
        res2$x <- rep(res$xmids, times=nybreaks-1)
        res2$y <- rep(res$ymids, each=nxbreaks-1)
        res2$f <- as.vector(res$result)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    res
}

binAverage <- function(x, y, xmin, xmax, xinc)
{
    if (missing(y))
        stop("must supply 'y'")
    if (missing(xmin))
        xmin <- min(as.numeric(x), na.rm=TRUE)
    if (missing(xmax))
        xmax <- max(as.numeric(x), na.rm=TRUE)
    if (missing(xinc))
        xinc  <- (xmax - xmin) / 10 
    if (xmax <= xmin)
        stop("must have xmax > xmin")
    if (xinc <= 0)
        stop("must have xinc > 0")
    xx <- head(seq(xmin, xmax, xinc), -1) + xinc / 2
    #cat("xx:", xx, "\n")
    nb <- length(xx)
    ##dyn.load("bin_average.so") # include this whilst debugging
    yy <- .C("bin_average", length(x), as.double(x), as.double(y),
             as.double(xmin), as.double(xmax), as.double(xinc),
             ##means=double(nb), NAOK=TRUE)$means
             means=double(nb), NAOK=TRUE, PACKAGE="oce")$means # include this whilst debugging
    list(x=xx, y=yy)
}



ungrid <- function(x, y, grid)
{
    nrow <- nrow(grid)
    ncol <- ncol(grid)
    grid <- as.vector(grid) # by columns
    x <- rep(x, times=ncol)
    y <- rep(y, each=nrow)
    ok <- !is.na(grid)
    list(x=x[ok], y=y[ok], grid=grid[ok])
}

approx3d <- function(x, y, z, f, xout, yout, zout) {
    equispaced <- function(x) sd(diff(x)) / mean(diff(x)) < 1e-5
    if (missing(x)) stop("must provide x")
    if (missing(y)) stop("must provide y")
    if (missing(z)) stop("must provide z")
    if (missing(f)) stop("must provide f")
    if (missing(xout)) stop("must provide xout")
    if (missing(yout)) stop("must provide yout")
    if (missing(zout)) stop("must provide zout")
    if (!equispaced(x)) stop("x values must be equi-spaced")
    if (!equispaced(y)) stop("y values must be equi-spaced")
    if (!equispaced(z)) stop("z values must be equi-spaced")
    .Call("approx3d", x, y, z, f, xout, yout, zout)
}

errorbars <- function(x, y, xe, ye, percent=FALSE, style=0, length=0.025, ...)
{
    if (missing(x))
        stop("must supply x")
    if (missing(y))
        stop("must supply y")
    n <- length(x)
    if (n != length(y))
        stop("x and y must be of same length\n")
    if (missing(xe) && missing(ye))
        stop("must give either xe or ye")
    if (1 == length(xe))
        xe <- rep(xe, n) # FIXME probably gives wrong length
    if (1 == length(ye))
        ye <- rep(ye, n)
    if (!missing(xe)) {
        if (n != length(xe))
            stop("x and xe must be of same length\n")
        if (percent)
            xe <- xe * x / 100
        look <- xe != 0
        if (style == 0) {
            segments(x[look], y[look], x[look]+xe[look], y[look], ...)
            segments(x[look], y[look], x[look]-xe[look], y[look], ...)
        } else if (style == 1) {
            arrows(x[look], y[look], x[look] + xe[look], y[look], angle=90, length=length, ...)
            arrows(x[look], y[look], x[look] - xe[look], y[look], angle=90, length=length, ...)
        } else {
            stop("unknown value ", style, " of style; must be 0 or 1\n")
        }
    }
    if (!missing(ye)) {
        if (n != length(ye))
            stop("y and ye must be of same length\n")
        if (percent)
            ye <- ye * y / 100
        look <- ye != 0
        if (style == 0) {
            segments(x[look], y[look], x[look], y[look]+ye[look], ...)
            segments(x[look], y[look], x[look], y[look]-ye[look], ...)
        } else if (style == 1) {
            arrows(x[look], y[look], x[look], y[look] + ye[look], angle=90, length=length, ...)
            arrows(x[look], y[look], x[look], y[look] - ye[look], angle=90, length=length, ...)
        } else {
            stop("unknown value ", style, " of style; must be 0 or 1\n")
        }
    }
}

findInOrdered <- function(x, f)
{
    if (missing(x))
        stop("'x' missing")
    if (missing(f))
        stop("'f' missing")
    .Call("bisect", x, f)
}

filterSomething <- function(x, filter)
{
    if (is.raw(x)) {
        x <- as.numeric(x)
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- as.integer(filter(x, filter))
        res <- ifelse(res < 0, 0, res)
        res <- ifelse(res > 255, 255, res)
        res <- as.raw(res)
    } else {
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- filter(x, filter)
    }
    res
}

plotTaylor <- function(x, y, scale, pch, col, labels, pos, ...)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (is.vector(y))
        y <- matrix(y)
    ncol <- ncol(y)
    if (missing(pch))
        pch <- 1:ncol
    if (missing(col))
        col <- rep("black", ncol)
    haveLabels <- !missing(labels)
    if (missing(pos))
        pos <- rep(2, ncol)
    if (length(pos) < ncol)
        pos <- rep(pos[1], ncol)
    xSD <- sd(x, na.rm=TRUE)
    ySD <- sd(as.vector(y), na.rm=TRUE)
    if (missing(y)) stop("must supply 'y'")
    halfArc <- seq(0, pi, length.out=200)
    ## FIXME: use figure geometry, to avoid axis cutoff
    if (missing(scale))
        scale <- max(pretty(c(xSD, ySD)))
    plot.new()
    plot.window(c(-1.2, 1.2) * scale, c(0, 1.2) * scale, asp=1)
    ##plot.window(c(-1.1, 1.1), c(0.1, 1.2), asp=1)
    sdPretty <- pretty(c(0, scale))
    for (radius in sdPretty)
        lines(radius * cos(halfArc), radius * sin(halfArc), col='gray')
    ## spokes
    for (rr in seq(-1, 1, 0.2))
        lines(c(0, max(sdPretty)*cos(pi/2 + rr * pi / 2)),
              c(0, max(sdPretty)*sin(pi/2 + rr * pi / 2)), col='gray')
    axisLabels <- format(sdPretty)
    axisLabels[1] <- paste(0)
    axis(1, pos=0, at=sdPretty, labels=axisLabels)
    ## temporarily permit labels outside the platting zone
    xpdOld <- par('xpd')
    par(xpd=NA)
    m <- max(sdPretty)
    text(m, 0, "R=1", pos=4)
    text(0, m, "R=0", pos=3)
    text(-m, 0, "R=-1", pos=2)
    par(xpd=xpdOld)
    points(xSD, 0, pch=20, cex=1.5)
    for (column in 1:ncol(y)) {
        ySD <- sd(y[,column], na.rm=TRUE)
        R <- cor(x, y[,column])^2
        ##cat("column=", column, "ySD=", ySD, "R=", R, "col=", col[column], "pch=", pch[column], "\n")
        xx <- ySD * cos((1 - R) * pi / 2)
        yy <- ySD * sin((1 - R) * pi / 2)
        points(xx, yy, pch=pch[column], lwd=2, col=col[column], cex=2)
        if (haveLabels) {
            ##cat(labels[column], "at", pos[column], "\n")
            text(xx, yy, labels[column], pos=pos[column], ...)
        }
    }
}

prettyPosition <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "prettyPosition(...) {\n", sep="", unindent=1)
    r <- diff(range(x, na.rm=TRUE))
    oceDebug(debug, 'range(x)=', range(x), 'r=', r, '\n')
    if (r > 5) {                       # D only
        res <- pretty(x)
    } else if (r > 1) {                # round to 30 minutes
        res <- (1 / 2) * pretty(2 * x)
        oceDebug(debug, "case 1: res=", res, "\n")
    } else if (r > 30/60) {            # round to 1 minute, with extras
        res <- (1 / 60) * pretty(60 * x, n=6)
        oceDebug("case 2: res=", res, "\n")
    } else if (r > 5/60) {             # round to 1 minute
        res <- (1 / 60) * pretty(60 * x, 4)
        oceDebug(debug, "case 3: res=", res, "\n")
    } else if (r > 10/3600) {          # round to 10 sec
        res <- (1 / 360) * pretty(360 * x)
        oceDebug(debug, "case 4: res=", res, "\n")
    } else {                           # round to seconds
        res <- (1 / 3600) * pretty(3600 * x)
        if (debug) cat("case 5: res=", res, "\n")
    }
    oceDebug(debug, "} # prettyPosition\n", unindent=1)
    res
}

smoothSomething <- function(x, ...)
{
    if (is.raw(x)) {
        x <- as.numeric(x)
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- as.integer(smooth(x, ...))
        res <- ifelse(res < 0, 0, res)
        res <- ifelse(res > 255, 255, res)
        res <- as.raw(res)
    } else {
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- smooth(x, ...)
    }
    res
}


rescale <- function(x, xlow, xhigh, rlow=0, rhigh=1, clip=TRUE)
{
    x <- as.numeric(x)
    finite <- is.finite(x)
    ##r <- range(x, na.rm=TRUE)
    if (missing(xlow))
        xlow <- min(x, na.rm=TRUE)
    if (missing(xhigh))
        xhigh <- max(x, na.rm=TRUE)
    res <- rlow + (rhigh - rlow) * (x - xlow) / (xhigh - xlow)
    if (clip) {
        res <- ifelse(res < min(rlow, rhigh), rlow, res)
        res <- ifelse(res > max(rlow, rhigh), rhigh, res)
    }
    res[!finite] <- NA
    res
}

retime <- function(x, a, b, t0, debug=getOption("oceDebug"))
{
    if (missing(x))
        stop("must give argument 'x'")
    if (missing(a))
        stop("must give argument 'a'")
    if (missing(b))
        stop("must give argument 'b'")
    if (missing(t0))
        stop("must give argument 't0'")
    oceDebug(debug, paste("retime.adv(x, a=", a, ", b=", b, ", t0=\"", format(t0), "\")\n"),sep="", unindent=1)
    res <- x
    oceDebug(debug, "retiming x@data$time")
    res@data$time <- x@data$time + a + b * (as.numeric(x@data$time) - as.numeric(t0))
    if ("timeSlow" %in% names(x@data)) {
        oceDebug(debug, "retiming x@data$timeSlow\n")
        res@data$timeSlow <- x@data$timeSlow + a + b * (as.numeric(x@data$timeSlow) - as.numeric(t0))
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # retime.adv()\n", unindent=1)
    res
}

threenum <- function(x)
{
    dim <- dim(x)
    if (is.character(x) || is.null(x)) {
        res <- rep(NA, 3)
    } else if (is.list(x)) {
        if (2 == sum(c("lsb", "msb") %in% names(x))) { # e.g. landsat data
            x <- as.numeric(x$lsb) + 256 * as.numeric(x$msb)
            dim(x) <- dim
            res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
        } else {
            res <- rep(NA, 3)
        }
    } else if (is.raw(x)) {
        x <- as.numeric(x)
        dim(x) <- dim
        res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
    } else if (is.factor(x)) {
        res <- rep(NA, 3)
    } else if (sum(!is.na(x))) {
        res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
    } else {
        res <- rep(NA, 3)
    }
    ## 20160314: tack on dimensions, neccessitating conversion to character
    res[1] <- format(res[1]) # do these independently
    res[2] <- format(res[2])
    res[3] <- format(res[3])
    if (is.array(x)) {
        res <- c(res, paste(dim(x), collapse="x"))
    } else {
        res <- c(res, format(length(x)))
    }
    res
}

## threenum <- function(x)
## {
##     if (is.character(x) || is.null(x)) {
##         res <- rep(NA, 3)
##     } else if (is.list(x)) {
##         if (2 == sum(c("lsb", "msb") %in% names(x))) { # e.g. landsat data
##             x <- as.numeric(x$lsb) + 256 * as.numeric(x$msb)
##             res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
##         } else {
##             res <- rep(NA, 3)
##         }
##     } else if (is.raw(x)) {
##         x <- as.numeric(x)
##         res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
##     } else if (is.factor(x)) {
##         res <- rep(NA, 3)
##     } else if (sum(!is.na(x))) {
##         res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
##     } else {
##         res <- rep(NA, 3)
##     }
##     res
## }

normalize <- function(x)
{
    var <- var(x, na.rm=TRUE)
    if (var == 0)
        rep(0, length(x))
    else
        (x - mean(x, na.rm=TRUE)) / sqrt(var)
}

detrend <- function(x, y)
{
    if (missing(x))
        stop("must give x")
    n <- length(x)
    if (missing(y)) {
        y <- x
        x <- seq_along(y)
    } else {
        if (length(y) != n)
            stop("x and y must be of same length, but they are ", n, " and ", length(y))
    }
    if (x[1] == x[n])
        stop("cannot have x[1] == x[n]")
    b <- (y[1] - y[n]) / (x[1] - x[n])
    a <- y[1] - b * x[1]
    list(Y=y-(a+b*x), a=a, b=b)
}

despike <- function(x, reference=c("median", "smooth", "trim"), n=4, k=7, min=NA, max=NA,
                    replace=c("reference","NA"), skip)
{
    if (is.vector(x)) {
        x <- despikeColumn(x, reference=reference, n=n, k=k, min=min, max=max, replace=replace)
    } else {
        if (missing(skip)) {
            if (inherits(x, "ctd"))
                skip <- c("time", "scan", "pressure")
            else
                skip <- NULL
        }
        if (inherits(x, "oce")) {
            columns <- names(x@data)
            for (column in columns) {
                if (!(column %in% skip)) {
                    x[[column]] <- despikeColumn(x[[column]],
                                                 reference=reference, n=n, k=k, min=min, max=max, replace=replace)
                }
            }
            x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        } else {
            columns <- names(x)
            for (column in columns) {
                if (!(column %in% skip)) {
                    x[[column]] <- despikeColumn(x[[column]],
                                                 reference=reference, n=n, k=k, min=min, max=max, replace=replace)
                }
            }
        }
    }
    x
}

despikeColumn <- function(x, reference=c("median", "smooth", "trim"), n=4, k=7, min=NA, max=NA,
                          replace=c("reference","NA"))
{
    reference <- match.arg(reference)
    replace <- match.arg(replace)
    gave.min <- !is.na(min)
    gave.max <- !is.na(max)
    nx <- length(x)
    ## degap
    na <- is.na(x)
    if (sum(na) > 0) {
        i <- 1:nx
        x.gapless <- approx(i[!na], x[!na], i)$y
    } else {
        x.gapless <- x
    }
    if (reference == "median" || reference == "smooth") {
        if (reference == "median")
            x.reference <- runmed(x.gapless, k=k)
        else
            x.reference <- as.numeric(smooth(x.gapless))
        distance <- abs(x.reference - x.gapless)
        stddev <- sqrt(var(distance))
        bad <- distance > n * stddev
        nbad <- sum(bad)
        if (nbad > 0) {
            if (replace == "reference")
                x[bad] <- x.reference[bad]
            else
                x[bad] <- rep(NA, nbad)
        }
    } else if (reference == "trim") {
        if (!gave.min || !gave.max)
            stop("must give min and max")
        bad <- !(min <= x & x <= max)
        nbad <- length(bad)
        if (nbad > 0) {
            i <- 1:nx
            if (replace == "reference")
                x[bad] <- approx(i[!bad], x.gapless[!bad], i[bad])$y
            else
                x[bad] <- rep(NA, nbad)
        }
    } else {
        stop("unknown reference ", reference)
    }
    x
}
rangeLimit <- function(x, min, max)
{
    if (missing(min) && missing(max)) {
        minmax <- quantile(x, 0.005, 0.995)
        min <- minmax[1]
        max <- minmax[2]
    }
    ifelse(max < x | x < min, NA, x)
}
unabbreviateYear <- function(year)
{
    ## handle e.g. 2008 as 2008 (full year), 8 (year-2000 offset), or 108 (year 1900 offset)
    ##cat("year[1]=",year[1])
    ##res <- ifelse(year > 1800, year, ifelse(year > 100, year + 1900, year + 2000))
    ##cat(" became ", res[1], "\n")
    ##res
    ifelse(year > 1800, year, ifelse(year > 50, year + 1900, year + 2000))
}

angleRemap <- function(theta)
{
    toRad <- atan2(1, 1) / 45
    atan2(sin(toRad * theta), cos(toRad * theta)) / toRad
}

unwrapAngle <- function(angle)
{
    toRad <- atan2(1, 1) / 45
    angle <- angle * toRad
    S <- sin(angle)
    C <- cos(angle)
    Smean <- mean(S, na.rm=TRUE)
    Smedian <- median(S, na.rm=TRUE)
    Cmean <- mean(C, na.rm=TRUE)
    Cmedian <- median(C, na.rm=TRUE)
    resMean <- atan2(Smean, Cmean)/toRad
    resMedian <- atan2(Smedian, Cmedian)/toRad
    resMean <- if (resMean < 0) resMean + 360 else resMean
    resMedian <- if (resMedian < 0) resMedian + 360 else resMedian
    list(mean=resMean, median=resMedian)
}

oce.pmatch <- function(x, table, nomatch=NA_integer_, duplicates.ok=FALSE)
{
    ## FIXME: do element by element, and extend as follows, to allow string numbers
    ## if (1 == length(grep("^[0-9]*$", ww))) which2[w] <- as.numeric(ww)
    if (is.numeric(x)) { 
        return(x)
    } else if (is.character(x)) {
        nx <- length(x)
        res <- NULL
        for (i in 1:nx) {
            if (1 == length(grep("^[0-9]*$", x[i]))) {
                res <- c(res, as.numeric(x[i]))
            } else {
                ix <- pmatch(x[i], names(table), nomatch=nomatch, duplicates.ok=duplicates.ok)
                ## use unlist() to expand e.g. list(x=1:10)
                res <- c(res, if (is.na(ix)) NA else unlist(table[[ix]]))
            }
        }
        ##m <- pmatch(x, names(table), nomatch=nomatch, duplicates.ok=duplicates.ok)
        ##return(as.numeric(table)[m])
        return(as.numeric(res))
    } else {
        stop("'x' must be numeric or character")
    }
}
ocePmatch <- oce.pmatch

oce.spectrum <- function(x, ...)
{
    args <- list(...)
    want.plot <- FALSE
    if ("plot" %in% names(args)) {
        want.plot <- args$plot
        args$plot <- FALSE
        args$x <- x
        res <- do.call(spectrum, args)
    }
    dt <- diff(res$freq[1:2])
    normalize <- var(x) / (sum(res$spec) * dt)
    res$spec <- normalize * res$spec
    if (want.plot)
        plot(res)
    invisible(res)
}
oceSpectrum <- oce.spectrum

vectorShow <- function(v, msg, digits=5)
{
    n <- length(v)
    if (missing(msg))
        msg <- deparse(substitute(v))
    if (n == 0) {
        paste(msg, "(empty vector)\n")
    } else {
        if (is.numeric(v)) {
            if (n > 4) {
                vv <- format(v[c(1, 2, n-1, n)], digits=digits)
                paste(msg, ": ", vv[1], ", ", vv[2], ", ..., ", vv[3], ", ", vv[4], " (length ", n, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(format(v, digits=digits), collapse=", "), "\n", sep="")
            }
        } else {
            if (n > 4) {
                paste(msg, ": ", v[1], ", ", v[2], ", ..., ", v[n-1], ", ", v[n], " (length ", n, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(v, collapse=", "), "\n", sep="")
            }
        }
    }
}

fullFilename <- function(filename)
{
    warn <- options('warn')$warn
    options(warn=-1)
    res <- normalizePath(filename)
    options(warn=warn)
    res
}


resizableLabel <- function(item, axis, sep, unit=NULL)
{
    if (missing(item))
        stop("must provide 'item'")
    if (missing(axis))
        axis <- "x"
    if (axis != "x" && axis != "y")
        stop("axis must be \"x\" or \"y\"")
    itemAllowed <- c("S", "C", "conductivity mS/cm", "conductivity S/m", "T",
                     "theta", "sigmaTheta", "conservative temperature",
                     "absolute salinity", "nitrate", "nitrite",
                     "oxygen", "oxygen saturation", "oxygen mL/L", "oxygen umol/L", "oxygen umol/kg",
                     "phosphate", "silicate", "tritium", "spice",
                     "fluorescence", "p", "z", "distance", "distance km",
                     "along-track distance km", "heading", "pitch", "roll", "u",
                     "v", "w", "speed", "direction", "eastward", "northward",
                     "depth", "elevation", "latitude", "longitude", "frequency cph",
                     "spectral density m2/cph")
    if (!missing(unit)) {
        unit <- unit[[1]] # removes the expression() from the bquote
    }
    iitem <- pmatch(item, itemAllowed)
    if (is.na(iitem))
        stop("item=\"", item, "\" is not allowed; try one of: \"",
             paste(itemAllowed, collapse="\", \""), "\"", sep="")
    item <- itemAllowed[iitem[1]]
    if (getOption("oceUnitBracket") == "[") {
        L <- " [" 
        R <- "]" 
    } else {
        L <- " ("
        R <- ")"
    }
    if (missing(sep)) {
        tmp <- getOption("oceUnitSep")
        sep <- if (!is.null(tmp)) tmp else ""
    }
    L <- paste(L, sep, sep="")
    R <- paste(sep, R, sep="")
    if (item == "T") {
        var <- gettext("Temperature", domain="R-oce")
        full <- bquote(.(var)*.(L)*degree*"C"*.(R))
        abbreviated <- bquote("T"*.(L)*degree*"C"*.(R))
    } else if (item == "conductivity mS/cm") {
        var <- gettext("Conductivity", domain="R-oce")
        full <- bquote(.(var)*.(L)*mS/cm*.(R))
        abbreviated <- bquote("C"*.(L)*mS/cm*.(R))
    } else if (item == "conductivity S/m") {
        var <- gettext("Conductivity", domain="R-oce")
        full <- bquote(.(var)*.(L)*S/m*.(R))
        abbreviated <- bquote("C"*.(L)*S/m*.(R))
    } else if (item == "C") { # unitless form
        var <- gettext("Conductivity Ratio", domain="R-oce")
        unit <- gettext("unitless", domain="R-oce")
        full <- bquote(.(var)*.(L)*.(unit)*.(R))
        abbreviated <- bquote("C")
    } else if (item == "conservative temperature") {
        var <- gettext("Conservative Temperature", domain="R-oce")
        full <- bquote(.(var)*.(L)*degree*"C"*.(R))
        abbreviated <- bquote(Theta*.(L)*degree*"C"*.(R))
    } else if (item == "sigmaTheta") {
        var <- gettext("Potential density anomaly", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[theta]*.(L)*kg/m^3*.(R))
    } else if (item == "theta") {
        var <- gettext("Potential Temperature", domain="R-oce")
        full <- bquote(.(var)*.(L)*degree*"C"*.(R))
        abbreviated <- bquote(theta*.(L)*degree*"C"*.(R))
    } else if (item == "tritium") {
        var <- gettext("Tritium", domain="R-oce")
        full <- bquote(.(var)*.(L)*Tu*.(R))
        abbreviated <- bquote(phantom()^3*H*.(L)*Tu*.(R))
    } else if (item ==  "nitrate") {
        var <- gettext("Nitrate", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(N*O[3]*.(L)*mu*mol/kg*.(R))
    } else if (item ==  "nitrite") {
        var <- gettext("Nitrite", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(N*O[2]*.(L)*mu*mol/kg*.(R))
    } else if (item ==  "oxygen") {
        ## Until 2015-12-12 the default unit was umol/kg
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var))
        abbreviated <- bquote(O[2])
    } else if (item ==  "oxygen saturation") {
        var <- gettext("Oxygen saturation", domain="R-oce")
        full <- bquote(.(var))
        abbreviated <- bquote(O[2]*.(L)*percent*saturation*.(R))
    } else if (item ==  "oxygen mL/L") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mL/L*.(R))
        abbreviated <- bquote(O[2]*.(L)*mL/L*.(R))
    } else if (item ==  "oxygen umol/L") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/L*.(R))
        abbreviated <- bquote(O[2]*.(L)*mu*mol/L*.(R))
    } else if (item ==  "oxygen umol/kg") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(O[2]*.(L)*mu*mol/kg*.(R))
    } else if (item ==  "phosphate") {
        var <- gettext("Phosphate", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(P*O[4]*.(L)*mu*mol/kg*.(R))
    } else if (item ==  "silicate") {
        var <- gettext("Silicate", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(Si*O[4]*.(L)*mu*mol/kg*.(R))
    } else if (item == "fluorescence") {
        var <- gettext("Fluorescence", domain="R-oce")
        ## FIXME: need to consider units
        abbreviated <- full <- bquote(.(var))
    } else if (item == "spice") {
        var <- gettext("Spice", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*kg/m^3*.(R))
    } else if (item == "S") {
        full <- gettext("Practical Salinity", domain="R-oce")
        abbreviated <- expression(S)
    } else if (item == "absolute salinity") {
        var <- gettext("Absolute Salinity", domain="R-oce")
        full <- bquote(.(var)*.(L)*g/kg*.(R))
        abbreviated <- bquote(S[A]*.(L)*g/kg*.(R))
    } else if (item == "p") {
        var <- gettext("Pressure", domain="R-oce")
        full <- bquote(.(var)*.(L)*dbar*.(R))
        abbreviated <- bquote("p"*.(L)*dbar*.(R))
    } else if (item == "z") {
        var <- "z"
        abbreviated <- full <- bquote("z"*.(L)*m*.(R))
    } else if (item == "distance") {
        var <- gettext("Distance", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m*.(R))
    } else if (item == "distance km") {
        var <- gettext("Distance", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*km*.(R))
    } else if (item == "along-track distance km") {
        var <- gettext("Along-track Distance", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*km*.(R))
    } else if (item == "heading") {
        var <- gettext("Heading", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*degree*.(R))
    } else if (item == "pitch") {
        var <- gettext("Pitch", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*degree*.(R))
    } else if (item == "roll") {
        var <- gettext("Roll", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*degree*.(R))
    } else if (item == "u" || item == "v" || item == "w") {
        abbreviated <- full <- bquote(.(item)*.(L)*m/s*.(R))
    } else if (item == "eastward") {
        var <- gettext("Eastward", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m/s*.(R))
    } else if (item == "northward") {
        var <- gettext("Northward", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m/s*.(R))
    } else if (item == "depth") {
        var <- gettext("Depth", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m*.(R))
    } else if (item == "elevation") {
        var <- gettext("Elevation", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m*.(R))
    } else if (item ==  "speed") {
        var <- gettext("Speed", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*m/s*.(R))
    } else if (item == "latitude") {
        var <- gettext("Latitude", domain="R-oce")
        ## maybe add deg "N" "S" etc here, but maybe not (aesthetics)
        abbreviated <- full <- var
    } else if (item == "longitude") {
        var <- gettext("Longitude", domain="R-oce")
        ## maybe add deg "E" "W" etc here, but maybe not (aesthetics)
        abbreviated <- full <- var
    } else if (item == "frequency cph") {
        var <- gettext("Frequency", domain="R-oce")
        unit <- gettext("cph", domain="R-oce")
        abbreviated <- full <- bquote(.(var)*.(L)*.(unit)*.(R))
    } else if (item == "spectral density m2/cph") {
        var <- gettext("Spectral density", domain="R-oce")
        full <- bquote(.(var)*.(L)*m^2/cph*.(R))
        var <- gettext("Spec. dens.", domain="R-oce")
        abbreviated <- bquote(.(var)*.(L)*m^2/cph*.(R))
    }
    spaceNeeded <- strwidth(paste(full, collapse=""), "inches")
    whichAxis <- if (axis == "x") 1 else 2
    spaceAvailable <- abs(par("fin")[whichAxis])
    fraction <- spaceNeeded / spaceAvailable
    ##cat("pin=", par('pin'), "\n")
    ##cat("spaceNeeded: in inches:", spaceNeeded, "\n")
    ##cat("whichAxis=", whichAxis, "\n")
    ##cat("spaceAvailable=", spaceAvailable, "\n")
    ##cat("fraction=", fraction, "\n")
    #print(full)
    #print(abbreviated)
    if (fraction < 1) full else abbreviated
}

latlonFormat <- function(lat, lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    res <- vector("character", n)
    for (i in 1:n) {
        if (is.na(lat[i]) || is.na(lon[i]))
            res[i] <- "Lat and lon unknown"
        else
            res[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N  " else "S  ",
                             format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "W",
                             sep="")
    }
    res
}

latFormat <- function(lat, digits=max(6, getOption("digits") - 1))
{
    n <- length(lat)
    if (n < 1) return("")
    res <- vector("character", n)
    for (i in 1:n) {
        if (is.na(lat[i]))
            res[i] <-  ""
        else
            res[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N" else "S", sep="")
    }
    res
}

lonFormat <- function(lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    if (n < 1) return("")
    res <- vector("character", n)
    for (i in 1:n)
        if (is.na(lon[i]))
            res[i] <- ""
        else
            res[i] <- paste(format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "S",
                             sep="")
    res
}

GMTOffsetFromTz <- function(tz)
{
    ## Data are from
    ##   http://www.timeanddate.com/library/abbreviations/timezones/
    ## and hand-edited, so there may be errors.  Also, note that some of these
    ## contradict ... I've commented out conflicting definitions that I think
    ## will come up most rarely in use, but perhaps something better should
    ## be devised.  (Maybe this is not a problem.  Maybe only MEDS uses these,
    ## as opposed to GMT offsets, and maybe they only work in 5 zones, anyway...)
    if (tz == "A"   )   return( -1  ) # Alpha Time Zone Military        UTC + 1 hour
    if (tz == "ACDT")   return(-10.5) # Australian Central Daylight Time   Australia    UTC + 10:30 hours
    if (tz == "ACST")   return( -9.5) # Australian Central Standard Time  Australia     UTC + 9:30 hours
    if (tz == "ADT" )   return( 3   ) # Atlantic Daylight Time  North America   UTC - 3 hours
    if (tz == "AEDT")   return(-11  ) # Aus. East. Day. Time or Aus. East Summer Time Aus. UTC + 11 hours
    if (tz == "AEST")   return(-10  ) # Australian Eastern Standard Time  Australia UTC + 10 hours
    if (tz == "AKDT")   return(  8  ) # Alaska Daylight Time    North America   UTC - 8 hours
    if (tz == "AKST")   return(  9  ) # Alaska Standard Time    North America   UTC - 9 hours
    if (tz == "AST" )   return(  4  ) # Atlantic Standard Time  North America   UTC - 4 hours
    if (tz == "AWDT")   return( -9  ) # Australian Western Daylight Time        Australia       UTC + 9 hours
    if (tz == "AWST")   return( -8  ) # Australian Western Standard Time        Australia       UTC + 8 hours
    if (tz == "B"   )   return( -2  ) # Bravo Time Zone Military        UTC + 2 hours
    if (tz == "BST" )   return( -1  ) # British Summer Time     Europe  UTC + 1 hour
    if (tz == "C"   )   return( -3  ) # Charlie Time Zone       Military        UTC + 3 hours
    ##if (tz == "CDT")  return(-10.5) # Central Daylight Time   Australia       UTC + 10:30 hours
    if (tz == "CDT" )   return(  5  ) # Central Daylight Time   North America   UTC - 5 hours
    if (tz == "CEDT")   return( -2  ) # Central European Daylight Time  Europe  UTC + 2 hours
    if (tz == "CEST")   return( -2  ) # Central European Summer Time    Europe  UTC + 2 hours
    if (tz == "CET" )   return( -1  ) # Central European Time   Europe  UTC + 1 hour
    ##if (tz == "CST" ) return(-10.5) # Central Summer Time     Australia       UTC + 10:30 hours
    ##if (tz == "CST" ) return( -9.5) # Central Standard Time   Australia       UTC + 9:30 hours
    if (tz == "CST" )   return(  6  ) # Central Standard Time   North America   UTC - 6 hours
    if (tz == "CXT" )   return( -7  ) # Christmas Island Time   Australia       UTC + 7 hours
    if (tz == "D"   )   return( -4  ) # Delta Time Zone Military        UTC + 4 hours
    if (tz == "E"   )   return( -5  ) # Echo Time Zone  Military        UTC + 5 hours
    ##if (tz == "EDT" ) return( -11 ) # Eastern Daylight Time   Australia       UTC + 11 hours
    if (tz == "EDT" )   return(  4  ) # Eastern Daylight Time   North America   UTC - 4 hours
    if (tz == "EEDT")   return( -3  ) # Eastern European Daylight Time  Europe  UTC + 3 hours
    if (tz == "EEST")   return( -3  ) # Eastern European Summer Time    Europe  UTC + 3 hours
    if (tz == "EET")    return( -2  ) # Eastern European Time   Europe  UTC + 2 hours
    ##if (tz == "EST")  return( -11 ) # Eastern Summer Time     Australia       UTC + 11 hours
    ##if (tz == "EST")  return( -10 ) # Eastern Standard Time   Australia       UTC + 10 hours
    if (tz == "EST" )   return(  5  ) # Eastern Standard Time   North America   UTC - 5 hours
    if (tz == "F"   )   return( -6  ) # Foxtrot Time Zone       Military        UTC + 6 hours
    if (tz == "G"   )   return( -7  ) # Golf Time Zone  Military        UTC + 7 hours
    if (tz == "GMT" )   return(  0  ) # Greenwich Mean Time     Europe  UTC
    if (tz == "H"   )   return( -8  ) # Hotel Time Zone Military        UTC + 8 hours
    if (tz == "HAA" )   return(  3  ) # Heure Avancee de l'Atlantique   North America   UTC - 3 hours
    if (tz == "HAC" )   return(  5  ) # Heure Avancee du Centre North America   UTC - 5 hours
    if (tz == "HADT")   return(  9  ) # Hawaii-Aleutian Daylight Time   North America   UTC - 9 hours
    if (tz == "HAE" )   return(  4  ) # Heure Avancee de l'Est  North America   UTC - 4 hours
    if (tz == "HAP" )   return(  7  ) # Heure Avancee du Pacifique      North America   UTC - 7 hours
    if (tz == "HAR" )   return(  6  ) # Heure Avancee des Rocheuses     North America   UTC - 6 hours
    if (tz == "HAST")   return( 10  ) # Hawaii-Aleutian Standard Time   North America   UTC - 10 hours
    if (tz == "HAT" )   return(  2.5) # Heure Avancee de Terre-Neuve    North America   UTC - 2:30 hours
    if (tz == "HAY" )   return(  8  ) # Heure Avancee du Yukon  North America   UTC - 8 hours
    if (tz == "HNA" )   return(  4  ) # Heure Normaee de l'Atlantique   North America   UTC - 4 hours
    if (tz == "HNC" )   return(  6  ) # Heure Normale du Centre North America   UTC - 6 hours
    if (tz == "HNE" )   return(  5  ) # Heure Normale de l'Est  North America   UTC - 5 hours
    if (tz == "HNP" )   return(  8  ) # Heure Normale du Pacifique      North America   UTC - 8 hours
    if (tz == "HNR" )   return(  7  ) # Heure Normale des Rocheuses     North America   UTC - 7 hours
    if (tz == "HNT" )   return(  3.5) # Heure Normale de Terre-Neuve    North America   UTC - 3:30 hours
    if (tz == "HNY" )   return(  9  ) # Heure Normale du Yukon  North America   UTC - 9 hours
    if (tz == "I"   )   return( -9  ) # India Time Zone Military        UTC + 9 hours
    if (tz == "IST" )   return( -1  ) # Irish Summer Time       Europe  UTC + 1 hour
    if (tz == "K"   )   return(-10  ) # Kilo Time Zone  Military        UTC + 10 hours
    if (tz == "L"   )   return(-11  ) # Lima Time Zone  Military        UTC + 11 hours
    if (tz == "M"   )   return(-12  ) # Mike Time Zone  Military        UTC + 12 hours
    if (tz == "MDT" )   return(  6  ) # Mountain Daylight Time  North America   UTC - 6 hours
    if (tz == "MESZ")   return( -2  ) # Mitteleuroaische Sommerzeit     Europe  UTC + 2 hours
    if (tz == "MEZ" )   return( -1  ) # Mitteleuropaische Zeit  Europe  UTC + 1 hour
    if (tz == "MST" )   return(  7  ) # Mountain Standard Time  North America   UTC - 7 hours
    if (tz == "N"   )   return(  1  ) # November Time Zone      Military        UTC - 1 hour
    if (tz == "NDT" )   return(  2.5) # Newfoundland Daylight Time      North America   UTC - 2:30 hours
    if (tz == "NFT" )   return(-11.5) # Norfolk (Island) Time   Australia       UTC + 11:30 hours
    if (tz == "NST" )   return(  3.5) # Newfoundland Standard Time      North America   UTC - 3:30 hours
    if (tz == "O"   )   return(  1  ) # Oscar Time Zone Military        UTC - 2 hours
    if (tz == "P"   )   return(  3  ) # Papa Time Zone  Military        UTC - 3 hours
    if (tz == "PDT" )   return(  7  ) # Pacific Daylight Time   North America   UTC - 7 hours
    if (tz == "PST" )   return(  8  ) # Pacific Standard Time   North America   UTC - 8 hours
    if (tz == "Q"   )   return(  4  ) # Quebec Time Zone        Military        UTC - 4 hours
    if (tz == "R"   )   return(  4  ) # Romeo Time Zone Military        UTC - 5 hours
    if (tz == "S"   )   return(  6  ) # Sierra Time Zone        Military        UTC - 6 hours
    if (tz == "T"   )   return(  7  ) # Tango Time Zone Military        UTC - 7 hours
    if (tz == "U"   )   return(  8  ) # Uniform Time Zone       Military        UTC - 8 hours
    if (tz == "UTC" )   return(  0  ) # Coordinated Universal Time      Europe  UTC
    if (tz == "V"   )   return(  9  ) # Victor Time Zone        Military        UTC - 9 hours
    if (tz == "W"   )   return( 10  ) # Whiskey Time Zone       Military        UTC - 10 hours
    if (tz == "WDT" )   return( -9  ) # Western Daylight Time   Australia       UTC + 9 hours
    if (tz == "WEDT")   return( -1  ) # Western European Daylight Time  Europe  UTC + 1 hour
    if (tz == "WEST")   return( -1  ) # Western European Summer Time    Europe  UTC + 1 hour
    if (tz == "WET")    return(  0  ) # Western European Time   Europe  UTC
    ##if (tz == "WST")  return( -9  ) # Western Summer Time     Australia       UTC + 9 hours
    if (tz == "WST")    return( -8  ) # Western Standard Time   Australia       UTC + 8 hours
    if (tz == "X"  )    return( 11  ) # X-ray Time Zone Military        UTC - 11 hours
    if (tz == "Y"  )    return( 12  ) # Yankee Time Zone        Military        UTC - 12 hours
    if (tz == "Z"  )    return(  0  ) # Zulu Time Zone  Military        UTC
}

gravity <- function(latitude=45, degrees=TRUE)
{
    if (degrees) latitude <- latitude * 0.0174532925199433
    9.780318*(1.0+5.3024e-3*sin(latitude)^2-5.9e-6*sin(2*latitude)^2)
}

makeFilter <- function(type=c("blackman-harris", "rectangular", "hamming", "hann"), m, asKernel=TRUE)
{
    type <- match.arg(type)
    if (missing(m))
        stop("must supply 'm'")
    i <- seq(0, m - 1)
    if (type == "blackman-harris") {    # See Harris (1978) table on p65
        if (m == 2 * floor(m/2)) {
            m <- m + 1
            warning("increased filter length by 1, to make it odd")
        }
        a <- c(0.35875, 0.488829, 0.14128, 0.01168) # 4-term (-92dB) coefficients
        ff <- pi * i / (m - 1)
        coef <- a[1] - a[2]*cos(2*ff) + a[3]*cos(4*ff) - a[4]*cos(6*ff)
    } else if (type == "rectangular") {
        coef <- rep(1 / m, m)
    } else if (type == "hamming") {
        coef <- 0.54 - 0.46 * cos(2 * pi * i / (m-1))
    } else if (type == "hann") {
        coef <- 0.50 - 0.50 * cos(2 * pi * i / (m-1))
    }
    coef <- coef / sum(coef)           # ensure unit sum
    if (!asKernel)
        return(coef)
    if (m == 2 * floor(m/2))
        stop("m must be odd")
    middle <- ceiling(m / 2)
    coef <- coef[middle:m]
    return(kernel(coef=coef, name=paste(type, "(", m, ")", sep=""), r=0)) # the r=0 is to prevent code-analysis warning; it only applies to Fejer, which we do not use
}

oce.filter <- function(x, a=1, b, zero.phase=FALSE)
{
    if (missing(x))
        stop("must supply x")
    if (missing(b))
        stop("must supply b")
    if (!zero.phase) {
        return(.Call("oce_filter", x, a, b))
    } else {
        res <- .Call("oce_filter", x, a, b)
        res <- rev(res)
        res <- .Call("oce_filter", res, a, b)
        return(rev(res))
    }
}
oceFilter <- oce.filter


interpBarnes <- function(x, y, z, w,
                         xg, yg, xgl, ygl,
                         xr, yr, gamma=0.5, iterations=2, trim=0,
                         pregrid=FALSE,
                         debug=getOption("oceDebug"))
{
    debug <- max(0, min(debug, 2))
    oceDebug(debug, "interpBarnes(x, ...) {\n", unindent=1)
    if (!is.vector(x))
        stop("x must be a vector")
    n <- length(x)
    if (length(y) != n)
        stop("lengths of x and y disagree; they are ", n, " and ", length(y))
    if (length(z) != n)
        stop("lengths of x and z disagree; they are ", n, " and ", length(z))
    if (missing(w))
        w <- rep(1.0, length(x))
    if (missing(xg)) {
        if (missing(xgl)) {
            if (0 == diff(range(x, na.rm=TRUE))) {
                xg <- x[1]
            } else {
                xg <- pretty(x, n=50)
            }
        } else {
            xg <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=xgl)
        }
    }
    if (missing(yg)) {
        if (missing(ygl)) {
            if (0 == diff(range(y, na.rm=TRUE))) {
                yg <- y[1]
            } else {
                yg <- pretty(y, n=50)
            }
        } else {
            yg <- seq(min(y, na.rm=TRUE), max(y, na.rm=TRUE), length.out=ygl)
        }
    }
    if (missing(xr)) {
        xr <- diff(range(x, na.rm=TRUE)) / sqrt(n)
        if (xr == 0)
            xr <- 1
    }
    if (missing(yr)) {
        yr <- diff(range(y, na.rm=TRUE)) / sqrt(n)
        if (yr == 0)
            yr <- 1
    }
    ## Handle pre-gridding (code not DRY but short enough to be ok)
    if (is.logical(pregrid)) {
        if (pregrid) {
            pregrid <- c(4, 4)
            oceDebug(debug, "pregrid: ", paste(pregrid, collapse=" "))
            pg <- binMean2D(x, y, z,
                            xbreaks=seq(xg[1],tail(xg,1),(xg[2]-xg[1])/pregrid[1]),
                            ybreaks=seq(yg[1],tail(yg,1),(yg[2]-yg[1])/pregrid[2]),
                            flatten=TRUE)
            x <- pg$x
            y <- pg$y
            z <- pg$f
        }
    } else {
        if (!is.numeric(pregrid))
            stop("pregrid must be logical or a numeric vector")
        if (length(pregrid) < 0 || length(pregrid) > 2)
            stop("length(pregrid) must be 1 or 2")
        if (length(pregrid) == 1)
            pregrid <- rep(pregrid, 2)
        oceDebug(debug, "pregrid: ", paste(pregrid, collapse=" "))
        pg <- binMean2D(x, y, z,
                        xbreaks=seq(xg[1],tail(xg,1),(xg[2]-xg[1])/pregrid[1]),
                        ybreaks=seq(yg[1],tail(yg,1),(yg[2]-yg[1])/pregrid[2]),
                        flatten=TRUE)
        x <- pg$x
        y <- pg$y
        z <- pg$f
    }

    oceDebug(debug, "xg:", xg, "\n")
    oceDebug(debug, "yg:", yg, "\n")
    oceDebug(debug, "xr:", xr, "yr:", yr, "\n")
    oceDebug(debug, "gamma:", gamma, "iterations:", iterations, "\n")

    ok <- !is.na(x) & !is.na(y) & !is.na(z) & !is.na(w)
    g <- .Call("interp_barnes",
               as.double(x[ok]), as.double(y[ok]), as.double(z[ok]), as.double(w[ok]),
               as.double(xg), as.double(yg),
               as.double(xr), as.double(yr),
               as.double(gamma),
               as.integer(iterations))
    oceDebug(debug, "} # interpBarnes(...)\n", unindent=1)
    if (trim >= 0 && trim <= 1) {
        bad <- g$wg < quantile(g$wg, trim, na.rm=TRUE)
        g$zg[bad] <- NA
    }
    list(xg=xg, yg=yg, zg=g$zg, wg=g$wg, zd=g$zd)
}

coriolis <- function(lat, degrees=TRUE)
{
    ## Siderial day 86164.1 s.
    if (degrees) lat <- lat * 0.0174532925199433
    1.458423010785138e-4 * sin(lat)
}

undriftTime <- function(x, slowEnd = 0, tname="time")
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    names <- names(x@data)
    if (!(tname %in% names))
        stop("no column named '", tname, "'; only found: ", paste(names, collapse=" "))
    res <- x
    time <- res@data[[tname]]
    nt <- length(time)
    if (nt < 2) warning("too few data to to undrift time; returning object unaltered")
    else {
        sampleInterval <- as.numeric(difftime(time[2], time[1], units="s"))
        nt <- length(time)
        nt.out <- floor(0.5 + nt + slowEnd / sampleInterval)
        time.out <- seq.POSIXt(from=time[1], by=sampleInterval, length.out=nt.out)
        i <- seq(from=1, by=1, length.out=nt)
        i.out <- seq(from=1, to=nt, length.out = nt.out)
        out <- data.frame(array(dim=c(nt.out, length(x@data))))
        names(out) <- names
        out[[tname]] <- time.out
        for (name in names) {
            if (name != tname) {
                yy <- approx(x=i, y=x@data[[name]], xout=i.out)$y
                out[[name]] <- yy
            }
        }
        res@data <- out
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

fillGap <- function(x, method=c("linear"), rule=1)
{
    if (!is.numeric(x))
        stop("only works for numeric 'x'")
    method <- match.arg(method)
    class <- class(x)
    if (is.vector(x)) {
        res <- .Call("fillgap1d", as.numeric(x), rule)
    } else if (is.matrix(x))  {
        res <- x
        for (col in 1:ncol(x))
            res[,col] <- .Call("fillgap1d", as.numeric(x[,col]), rule)
        for (row in 1:nrow(x))
            res[row,] <- .Call("fillgap1d", as.numeric(x[row,]), rule)
    } else {
        stop("only works if 'x' is a vector or a matrix")
    }
    class(res) <-  class
    res
}


addColumn <- function (x, data, name)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (missing(data))
        stop("must supply data")
    if (missing(name))
        stop("must supply name")
    n <- length(data)
    nd <- length(x@data)
    if (n != length(data))
        stop("data length is ", n, " but it must be ", nd, " to match existing data")
    if (inherits(x, "ctd")) {
        res <- ctdAddColumn(x, data, name) # FIXME: supply units
    } else {
        res <- x
        res@data[[name]] <- data
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

decimate <- function(x, by=10, to, filter, debug=getOption("oceDebug"))
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    oceDebug(debug, "in decimate(x,by=", by, ",to=", if (missing(to)) "unspecified" else to, "...)\n")
    res <- x
    do.filter <- !missing(filter)
    if ("time" %in% names(x@data)) {
        if (missing(to))
            to <- length(x@data$time[[1]])
        if (length(by) == 1) { # FIXME: probably should not be here
            select <- seq(from=1, to=to, by=by)
            oceDebug(debug, vectorShow(select, "select:"))
        }
    }
    if (inherits(x, "adp")) {
        oceDebug(debug, "decimate() on an ADP object\n")
        warning("decimate(adp) not working yet ... just returning the adp unchanged")
        return(res) # FIXME
        ##nbeam <- dim(x@data$v)[3]
        for (name in names(x@data)) {
            oceDebug(debug, "decimating item named '", name, "'\n")
            if ("distance" == name)
                next
            if ("time" == name) {
                res@data[[name]] <- x@data[[name]][select]
            } else if (is.vector(x@data[[name]])) {
                oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                if (do.filter)
                    res@data[[name]] <- filterSomething(x@data[[name]], filter)
                res@data[[name]] <- res@data[[name]][select]
            } else if (is.matrix(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (j in 1: dim[2]) {
                    oceDebug(debug, "subsetting x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    if (do.filter) 
                        res@data[[name]][,j] <- filterSomething(x@data[[name]][,j], filter)
                    res@data[[name]][,j] <- res@data[[name]][,j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                print(dim)
                for (k in 1:dim[2]) {
                    for (j in 1:dim[3]) {
                        oceDebug(debug, "subsetting x@data[[", name, "]][", j, ",", k, "], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][,j,k] <- filterSomething(x@data[[name]][,j,k], filter)
                        res@data[[name]][,j,k] <- res@data[[name]][,j,k][select]
                    }
                }
            }
        }
    } else if (inherits(x, "adv")) { # FIXME: the (newer) adp code is probably better than this ADV code
        oceDebug(debug, "decimate() on an ADV object\n")
        warning("decimate(adv) not working yet ... just returning the adv unchanged")
        return(res) # FIXME
        for (name in names(x@data)) {
            if ("time" == name) {
                res@data[[name]] <- x@data[[name]][select]
            } else if (is.vector(x@data[[name]])) {
                oceDebug(debug, "decimating x@data$", name, ", which is a vector\n", sep="")
                if (do.filter)
                    res@data[[name]] <- filterSomething(x@data[[name]], filter)
                res@data[[name]] <- res@data[[name]][select]
            } else if (is.matrix(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (j in 1: dim[2]) {
                    oceDebug(debug, "decimating x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    if (do.filter) 
                        res@data[[name]][,j] <- filterSomething(x@data[[name]][,j], filter)
                    res@data[[name]][,j] <- res@data[[name]][,j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "decimating x@data[[", name, ",", j, ",", k, "]], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][,j,k] <- filterSomething(x@data[[name]][,j,k], filter)
                        res@data[[name]][,j,k] <- res@data[[name]][,j,k][select]
                    }
                }
            } else {
                stop("item data[[", name, "]] is not understood; it must be a vector, a matrix, or an array")
            }
        }
    } else if (inherits(x, "ctd")) {
        warning("decimate(ctd) not working yet ... just returning the ctd unchanged")
        return(res) # FIXME
        if (do.filter)
            stop("cannot (yet) filter ctd data during decimation") # FIXME
        select <- seq(1, dim(x@data)[1], by=by)
        res@data <- x@data[select,]
    } else if (inherits(x, "pt")) {
        warning("decimate(pt) not working yet ... just returning the pt unchanged")
        return(res) # FIXME
        if (do.filter)
            stop("cannot (yet) filter pt data during decimation") # FIXME
        for (name in names(res@data))
            res@data[[name]] <- x@data[[name]][select]
    } else if (inherits(x, "echosounder")) {
        oceDebug(debug, "decimate() on an 'echosounder' object\n")
        ## use 'by', ignoring 'to' and filter'
        if (length(by) != 2)
            stop("length(by) must equal 2.  First element is width of boxcar in pings, second is width in depths")
        by <- as.integer(by)
        byPing <- by[1]
        kPing <- as.integer(by[1])
        if (0 == kPing%%2)
            kPing <- kPing + 1
        byDepth <- by[2]
        kDepth <- as.integer(by[2])
        if (0 == kDepth%%2)
            kDepth <- kDepth + 1
        if (byDepth > 1) {
            depth <- x[["depth"]]
            a <- x[["a"]]
            ncol <- ncol(a)
            nrow <- nrow(a)
            ii <- 1:ncol
            depth2 <- binAverage(ii, depth, 1, ncol, byDepth)$y
            a2 <- matrix(nrow=nrow(a), ncol=length(depth2))
            for (r in 1:nrow)
                a2[r,] <- binAverage(ii, runmed(a[r,], kDepth), 1, ncol, byDepth)$y
            res <- x
            res[["depth"]] <- depth2
            res[["a"]] <- a2
            x <- res # need for next step
        }
        if (byPing > 1) {
            ##time <- x[["time"]]
            a <- x[["a"]]
            ncol <- ncol(a)
            nrow <- nrow(a)
            jj <- 1:nrow
            time2 <- binAverage(jj, as.numeric(x[["time"]]), 1, nrow, byPing)$y + as.POSIXct("1970-01-01 00:00:00", tz="UTC")
            a2 <- matrix(nrow=length(time2), ncol=ncol(a))
            for (c in 1:ncol)
                a2[,c] <- binAverage(jj, runmed(a[,c], kPing), 1, nrow, byPing)$y
            res <- x
            res[["time"]] <- time2
            res[["latitude"]] <- binAverage(jj, x[["latitude"]], 1, nrow, byPing)$y
            res[["longitude"]] <- binAverage(jj, x[["longitude"]], 1, nrow, byPing)$y
            res[["a"]] <- a2
        }
        ## do depth, rows of matrix, time, cols of matrix
    } else if (inherits(x, "topo")) {
        oceDebug(debug, "Decimating a topo object")
        lonlook <- seq(1, length(x[["longitude"]]), by=by)
        latlook <- seq(1, length(x[["latitude"]]), by=by)
        res[["longitude"]] <- x[["longitude"]][lonlook]
        res[["latitude"]] <- x[["latitude"]][latlook]
        res[["z"]] <- x[["z"]][lonlook, latlook]
    } else if (inherits(x, "landsat")) {
        oceDebug(debug, "Decimating a landsat object with by=", by, "\n")
        for (i in seq_along(x@data)) {
            b <- x@data[[i]]
            if (is.list(b)) {
                dim <- dim(b$msb)
                if (!is.null(dim))
                    res@data[[i]]$msb <- b$msb[seq(1, dim[1], by=by), seq(1, dim[2], by=by)] 
                dim <- dim(b$lsb)
                res@data[[i]]$lsb <- b$lsb[seq(1, dim[1], by=by), seq(1, dim[2], by=by)] 
            } else {
                dim <- dim(x@data[[i]])
                res@data[[i]] <- b[seq(1, dim[1], by=by), seq(1, dim[2], by=by)] 
            }
        }
    } else {
        stop("decimation does not work (yet) for objects of class ", paste(class(x), collapse=" "))
    }
    if ("deltat" %in% names(x@metadata)) # FIXME: should handle for individual cases, not here
        res@metadata$deltat <- by * x@metadata$deltat
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

oce.smooth <- function(x, ...)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    res <- x
    if (inherits(x, "adp")) {
        stop("cannot smooth ADP objects (feel free to request this from the author)")
    } else if (inherits(x, "adv")) {
        for (name in names(x@data)) {
            if (length(grep("^time", name)))
                next
            if (is.vector(x@data[[name]])) {
                oceDebug(debug, "smoothing x@data$", name, ", which is a vector\n", sep="")
                res@data[[name]] <- smooth(x@data[[name]], ...)
            } else if (is.matrix(x@data[[name]])) {
                for (j in 1: dim(x@data[[name]])[2]) {
                    oceDebug(debug, "smoothing x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    res@data[[name,j]] <- smooth(x@data[[name,j]], ...)
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "smoothing x@data[[", name, ",", j, "]], which is an arry \n", sep="")
                        res@data[[name,j,k]] <- smooth(x@data[[name,j,k]], ...)
                    }
                }
            }
        }
        warning("oce.smooth() has recently been recoded for 'adv' objects -- do not trust it yet!")
    } else if (inherits(x, "ctd")) {
        res <- x
        for (name in names(x@data))
            res@data[[name]] <- smooth(x@data[[name]], ...)
    } else {
        stop("smoothing does not work (yet) for objects of class ", paste(class(x), collapse=" "))
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}
oceSmooth <- oce.smooth

bcdToInteger <- function(x, endian=c("little", "big"))
{
    endian <- match.arg(endian)
    x <- as.integer(x)
    byte1 <- as.integer(floor(x / 16))
    byte2 <- x - 16 * byte1
    if (endian=="little") 10*byte1 + byte2 else byte1 + 10*byte2
}

byteToBinary <- function(x, endian=c("little", "big"))
{
    onebyte2binary <- function(x)
    {
        c("0000","0001","0010","0011",
          "0100","0101","0110","0111",
          "1000","1001","1010","1011",
          "1100","1101","1110","1111")[x+1]
    }
    endian <- match.arg(endian)
    res <- NULL
    if (class(x) == "raw")
        x <- readBin(x, "int", n=length(x), size=1, signed=FALSE)
    for (i in 1:length(x)) {
        if (x[i] < 0) {
            res <- c(res, "??")
        } else {
            ## FIXME: these are not bytes here; they are nibbles.  I don't think endian="little"
            ## makes ANY SENSE at all.  2012-11-22
            byte1 <- as.integer(floor(x[i] / 16))
            byte2 <- x[i] - 16 * byte1
            ##cat("input=",x[i],"byte1=",byte1,"byte2=",byte2,"\n")
            if (endian == "little")
                res <- c(res, paste(onebyte2binary(byte2), onebyte2binary(byte1), sep=""))
            else
                res <- c(res, paste(onebyte2binary(byte1), onebyte2binary(byte2), sep=""))
            ##cat(" res=",res,"\n")
        }
    }
    res
}

formatCI <- function(ci, style=c("+/-", "parentheses"), model, digits=NULL)
{
    formatCI.one <- function(ci, style, digits=NULL)
    {
        debug <- FALSE
        if (missing(ci))
            stop("must supply ci")
        ci <- as.numeric(ci)
        if (length(ci) == 3) {
            x <- ci[2]
            ci <- ci[c(1,3)]
        } else if (length(ci) == 2) {
            x <- mean(ci)
        } else {
            stop("ci must contain 2 or 3 elements")
        }
        sign <- sign(x)
        x <- abs(x)
        if (style == "+/-") {
            pm <- abs(diff(ci)/2)
            if (is.null(digits))
                paste(format(sign * x, digits=getOption("digits")), "+/-", format(pm, digits=getOption("digits")), sep="")
            else
                paste(format(sign * x, digits=digits), "+/-", format(pm, digits=digits), sep="")
        } else {
            pm <- abs(diff(ci)/2)
            scale <- 10^floor(log10(pm))
            pmr <- round(pm / scale)
            if (pmr == 10) {
                pmr <- 1
                scale <- scale * 10
            }
            ##scale <- 10^floor(log10(x))
            x0 <- x / scale
            ci0 <- ci / scale
            if (pm > x) return(paste(sign*x, "+/-", pm, sep=""))
            digits <- floor(log10(scale) + 0.1)
            if (digits < 0)
                fmt <- paste("%.", abs(digits), "f", sep="")
            else
                fmt <- "%.f"
            oceDebug(debug, "pm=", pm, ";pmr=", pmr, "; scale=", scale, "pm/scale=", pm/scale, "round(pm/scale)=", round(pm/scale), "\n", " x=", x,  "; x/scale=", x/scale, "digits=",digits,"fmt=", fmt, "\n")
            paste(sprintf(fmt, sign*x), "(", pmr, ")", sep="")
        }
    }
    style <- match.arg(style)
    if (!missing(model)) {
        cm <- class(model)
        ## > qt(0.6914619, 100000)
        ## [1] 0.5
        if (cm == "lm" || cm == "nls") {
            ci <- confint(model, level=0.6914619)
            names <- dimnames(ci)[[1]]
            n <- length(names)
            res <- matrix("character", nrow=n, ncol=1)
            rownames(res) <- names
            colnames(res) <- "value"
            for (row in 1:dim(ci)[1]) {
                res[row,1] <- formatCI.one(ci=ci[row,], style=style, digits=digits)
            }
        }
        res
    } else {
        if (missing(ci))
            stop("must give either ci or model")
        formatCI.one(ci=ci, style=style, digits=digits)
    }
}

integerToAscii <- function(i)
{
    c("", "\001", "\002", "\003", "\004", "\005", "\006", "\a", "\b",
      "\t", "\n", "\v", "\f", "\r", "\016", "\017", "\020", "\021",
      "\022", "\023", "\024", "\025", "\026", "\027", "\030", "\031",
      "\032", "\033", "\034", "\035", "\036", "\037", " ", "!", "\"",
      "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<",
      "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I",
      "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
      "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b",
      "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|",
      "}", "~", "\177", "\x80", "\x81", "\x82", "\x83", "\x84", "\x85",
      "\x86", "\x87", "\x88", "\x89", "\x8a", "\x8b", "\x8c", "\x8d",
      "\x8e", "\x8f", "\x90", "\x91", "\x92", "\x93", "\x94", "\x95",
      "\x96", "\x97", "\x98", "\x99", "\x9a", "\x9b", "\x9c", "\x9d",
      "\x9e", "\x9f", "\xa0", "\xa1", "\xa2", "\xa3", "\xa4", "\xa5",
      "\xa6", "\xa7", "\xa8", "\xa9", "\xaa", "\xab", "\xac", "\xad",
      "\xae", "\xaf", "\xb0", "\xb1", "\xb2", "\xb3", "\xb4", "\xb5",
      "\xb6", "\xb7", "\xb8", "\xb9", "\xba", "\xbb", "\xbc", "\xbd",
      "\xbe", "\xbf", "\xc0", "\xc1", "\xc2", "\xc3", "\xc4", "\xc5",
      "\xc6", "\xc7", "\xc8", "\xc9", "\xca", "\xcb", "\xcc", "\xcd",
      "\xce", "\xcf", "\xd0", "\xd1", "\xd2", "\xd3", "\xd4", "\xd5",
      "\xd6", "\xd7", "\xd8", "\xd9", "\xda", "\xdb", "\xdc", "\xdd",
      "\xde", "\xdf", "\xe0", "\xe1", "\xe2", "\xe3", "\xe4", "\xe5",
      "\xe6", "\xe7", "\xe8", "\xe9", "\xea", "\xeb", "\xec", "\xed",
      "\xee", "\xef", "\xf0", "\xf1", "\xf2", "\xf3", "\xf4", "\xf5",
      "\xf6", "\xf7", "\xf8", "\xf9", "\xfa", "\xfb", "\xfc", "\xfd",
      "\xfe", "\xff")[i+1]
}

applyMagneticDeclination <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "applyMagneticDeclination(x,declination=", declination, ") {\n", sep="", unindent=1)
    if (inherits(x, "cm")) {
        oceDebug(debug, "object is of type 'cm'\n")
        res <- x
        S <- sin(-declination * pi / 180)
        C <- cos(-declination * pi / 180)
        r <- matrix(c(C, S, -S, C), nrow=2)
        uvr <- r %*% rbind(x@data$u, x@data$v)
        res@data$u <- uvr[1,]
        res@data$v <- uvr[2,]
        oceDebug(debug, "originally, first u:", x@data$u[1:3], "\n")
        oceDebug(debug, "originally, first v:", x@data$v[1:3], "\n")
        oceDebug(debug, "after application, first u:", res@data$u[1:3], "\n")
        oceDebug(debug, "after application, first v:", res@data$v[1:3], "\n")
    } else {
        stop("cannot apply declination to object of class ", paste(class(x), collapse=", "), "\n")
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # applyMagneticDeclination\n", unindent=1)
    res
}

magneticField <- function(longitude, latitude, time)
{
    if (missing(longitude) || missing(latitude) || missing(time))
        stop("must provide longitude, latitude, and time")
    dim <- dim(latitude)
    if (!all(dim == dim(longitude)))
        stop("dimensions of longitude and latitude must agree")
    n <- length(latitude)
    if (inherits(time, "POSIXt")) {
        d <- as.POSIXlt(time)
        year <- d$year+1900
        yearday <- d$yday
        time <- year + yearday / 365.25 # ignore leap year issue (formulae not daily)
    }
    if (length(time) == 1) {
        time <- rep(time, n)
    } else {
        if (!all(dim == dim(time)))
            stop("dimensions of latitude and time must agree")
    }
    if (!is.null(dim)) {
        dim(longitude) <- n
        dim(latitude) <- n
        dim(time) <- n
    }
    ##isv <- 0
    ##itype <- 1                          # geodetic
    ##alt <- 0.0                          # altitude in km
    elong <- ifelse(longitude < 0, 360 + longitude, longitude)
    colat <- 90 - latitude
    r <- .Fortran("md_driver",
                  as.double(colat), as.double(elong), as.double(time),
                  as.integer(n),
                  declination=double(n),
                  inclination=double(n),
                  intensity=double(n))
    declination <- r$declination
    inclination <- r$inclination
    intensity <- r$intensity
    if (!is.null(dim)) {
        dim(declination) <- dim
        dim(inclination) <- dim
        dim(intensity) <- dim
    }
    list(declination=declination, inclination=inclination, intensity=intensity)
}


matchBytes <- function(input, b1, ...)
{
    if (missing(input))
        stop("must provide \"input\"")
    if (missing(b1))
        stop("must provide at least one byte to match")
    ##n <- length(input)
    dots <- list(...)
    lb <- 1 + length(dots)
    if (lb == 2)
        .Call("match2bytes", as.raw(input), as.raw(b1), as.raw(dots[[1]]), FALSE)
    else if (lb == 3)
        .Call("match3bytes", as.raw(input), as.raw(b1), as.raw(dots[[1]]), as.raw(dots[[2]]))
    else
        stop("must provide 2 or 3 bytes")
}


#' Rearrange areal matrix so Greenwich is near the centre
#'
#' Sometimes datasets are provided in matrix form, with first
#' index corresponding to longitudes ranging from 0 to 360.
#' \code{matrixShiftLongitude} cuts such matrices at
#' longitude=180, and swaps the pieces so that the dateline
#' is at the left of the matrix, not in the middle.
#'
#' @param m The matrix to be modified.
#' @param longitude A vector containing the longitude in the 0-360 convention. If missing, this is constructed to range from 0 to 360, with as many elements as the first index of \code{m}.
#'
#' @return A list containing \code{m} and \code{longitude}, both rearranged as appropriate.
#' @seealso \code{\link{shiftLongitude}} and \code{\link{standardizeLongitude}}.
matrixShiftLongitude <- function(m, longitude)
{
    if (missing(m))
        stop("must supply m")
    n <- dim(m)[1]
    if (missing(longitude))
        longitude <- seq.int(0, 360, length.out=n)
    if (n != length(longitude))
        stop("dim(m) and length(longitude) are incompatible")
    if (max(longitude, na.rm=TRUE) > 180) {
        cut <- which.min(abs(longitude-180))
        longitude <- c(longitude[seq.int(cut+1L, n)]-360, longitude[seq.int(1L, cut)])
        m <- m[c(seq.int(cut+1L, n), seq.int(1L, cut)),]
    }
    list(m=m, longitude=longitude)
}

matrixSmooth <- function(m, passes=1)
{
    if (missing(m))
        stop("must provide matrix 'm'")
    storage.mode(m) <- "double"
    if (passes > 0) {
        for (pass in seq.int(1, passes, 1)) {
            m <- .Call("matrix_smooth", m)
        }
    } else {
        warning("matrixSmooth given passes<=0, so returning matrix unmodified\n")
    }
    m
}

secondsToCtime <- function(sec)
{
    if (sec < 60)
        return(sprintf("00:00:%02d", sec))
    if (sec < 3600) {
        min <- floor(sec / 60)
        sec <- sec - 60 * min
        return(sprintf("00:%02d:%02d", min, sec))
    }
    hour <- floor(sec / 3600)
    sec <- sec - 3600 * hour
    min <- floor(sec / 60)
    sec <- sec - 60 * min
    return(sprintf("%02d:%02d:%02d", hour, min, sec))
}

ctimeToSeconds <- function(ctime)
{
    if (length(grep(":", ctime)) > 0) {
        parts <- as.numeric(strsplit(ctime, ":")[[1]])
        l <- length(parts)
        if (l == 1) s <- as.numeric(ctime)
        else if (l == 2) s <- parts[1] * 60 + parts[2]
        else if (l == 3) s <- parts[1] * 3600 + parts[2] * 60 + parts[3]
        else stop("cannot interpret \"time\"=", ctime, "as a time interval because it has more than 2 colons")
    } else {
        s <- as.numeric(ctime)
    }
    s
}

##showThrees <- function(x, indent="    ")
##{
##    if (!("threes" %in% names(x)))
##        stop("'x' has no item named 'threes'")
##    rownames <- rownames(x$threes)
##    colnames <- colnames(x$threes)
##    data.width <- max(nchar(colnames)) + 10
##    name.width <- max(nchar(rownames(x$threes))) + 4 # space for left-hand column
##    ncol <- length(colnames)
##    nrow <- length(rownames)
##    res <- indent
##    res <- paste(res, format(" ", width=1+name.width), collapse="")
##    res <- paste(res, paste(format(colnames, width=data.width, justify="right"), collapse=" "))
##    res <- paste(res, "\n", sep="")
##    digits <- max(5, getOption("digits") - 1)
##    for (irow in 1L:nrow) {
##        res <- paste(res, indent, format(rownames[irow], width=name.width), "  ", sep="") # FIXME: should not need the "  "
##        for (icol in 1L:ncol) {
##            res <- paste(res, format(x$threes[irow,icol], digits=digits, width=data.width, justify="right"), sep=" ")
##        }
##        res <- paste(res, "\n", sep="")
##    }
##    res
##}

oce.debug <- function(debug=0, ..., unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (n > 0)
            cat(paste(rep("  ", n), collapse=""))
        cat(...)
    }
    flush.console()
    invisible()
}
oceDebug <- oce.debug

showMetadataItem <- function(object, name, label="", postlabel="", isdate=FALSE, quote=FALSE)
{
    if (name %in% names(object@metadata)) {
        item <- object@metadata[[name]]
        if (isdate) item <- format(item)
        if (quote) item <- paste('`"', item, '"`', sep="")
        cat(paste("* ", label, item, postlabel, "\n", sep=""))
    }
}

integrateTrapezoid <- function(x, y, type=c("A", "dA", "cA"))
{
    if (missing(y)) {
        res <- .Call("trap", 1, x, switch(match.arg(type), A=0, dA=1, cA=2))
    } else {
        res <- .Call("trap", x, y, switch(match.arg(type), A=0, dA=1, cA=2))
    }
}

grad <- function(h, x, y)
{
    if (missing(h)) stop("must give h")
    if (missing(x)) stop("must give x")
    if (missing(y)) stop("must give y")
    if (length(x) != nrow(h)) stop("length of x (", length(x), ") must equal number of rows in h (", nrow(h), ")")
    if (length(y) != ncol(h)) stop("length of y (", length(y), ") must equal number of cols in h (", ncol(h), ")")
    .Call("gradient", h, as.double(x), as.double(y))
}

oce.as.raw <- function(x)
{       # prevent warnings from out-of-range with as.raw()
    na <- is.na(x)
    x[na] <- 0                 # FIXME: what to do here?
    x <- ifelse(x < 0, 0, x)
    x <- ifelse(x > 255, 255, x)
    x <- as.raw(x)
    x
}

oce.convolve <- function(x, f, end=2)
{
    .Call("oce_convolve", x, f, end)
}
oceConvolve <- oce.convolve


#' Try to guess data names from hints found in file headers
#'
#' @details
#' Interoperability between oce functions requires that standardized data names
#' be used, e.g. \code{"temperature"} for in-situ temperature. Very few
#' data-file headers name the temperature column in exactly that way, however,
#' and this function is provided to try to guess the names.
#'
#' @param names a vector of character strings with original names
#' @param scheme an optional indication of the scheme that is employed. This may
#' be \code{"ODF"}, in which case \code{\link{ODFNames2oceNames}} is used,
#' or \code{"met"}, in which case some tentative code for met files is used.
#'
#' @return
#' Vector of strings for the decoded names. If an unknown scheme is provided,
#' this will just be \code{names}.
decodeDataNames <- function(names, scheme)
{
    ##schemeGiven <- !missing(scheme)
    res <- names
    if (!missing(scheme)) {
        if (scheme == "ODF") {
            res <- ODFNames2oceNames(names)
        } else if (scheme == "met") {
            ## FIXME: capture the flags also
            if (1 == length(i <- grep("^Temp.*C.*$", res))) res[i] <- "temperature"
            if (1 == length(i <- grep("^Stn.*Press.*kPa.*$", res))) res[i] <- "pressure"
            if (1 == length(i <- grep("^Wind.*Spd.*km.*$", res))) res[i] <- "wind"
            if (1 == length(i <- grep("^Wind.*deg.*$", res))) res[i] <- "direction"
            if (1 == length(i <- grep("^Visibility.*km.*$", res))) res[i] <- "visibility"
            if (1 == length(i <- grep("^Rel\\.Hum\\.\\.\\.\\.$", res))) res[i] <- "humidity"
            if (1 == length(i <- grep("^Dew\\.Point\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "dewPoint"
            if (1 == length(i <- grep("^Wind\\.Chill$", res))) res[i] <- "windChill"
            if (1 == length(i <- grep("^Weather$", res))) res[i] <- "weather"
            if (1 == length(i <- grep("^Hmdx$", res))) res[i] <- "humidex"
        } else {
            warning("unknown scheme ", scheme)
        }
    } else {
        ## temperature
        col <- grep("temp", names, ignore.case=TRUE, useBytes=TRUE)
        if (1 == length(col))
            res[col] <- "temperature"
    }
    res
}

#' Remove leading and trailing whitespace from strings
#'
#' @param s vector of character strings
#' @return a new vector formed by trimming leading and trailing whitespace
#' from the elements of \code{s}.
trimString <- function(s)
{
    gsub("^ *", "", gsub(" *$", "", s))
}
