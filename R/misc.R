## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

## alphabetized functions START

abbreviateVector <- function(x)
{
    if (1 >= length(x)) {
        return(x)
    } else {
        ud <- unique(diff(x))
        if (1 == length(ud) && 1 == ud) return(paste(x[1], ":", tail(x, 1), sep="")) else return(x)
    }
}


#' Add a Column to the Data Slot of an Oce object [defunct]
#'
#' \strong{WARNING:} This function will be removed soon; see \link{oce-defunct}.
#'
#' Use \code{\link{oceSetData}} instead of the present function.
#'
#' @param x A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' @param data the data.  The length of this item must match that of the
#' existing data entries in the \code{data} slot).
#' @param name the name of the column.
#' @return An object of \code{\link[base]{class}} \code{oce}, with a new
#' column.
#' @author Dan Kelley
#' @seealso Please use \code{\link{oceSetData}} instead of the present function.
#' @family functions that will be removed soon
addColumn <- function (x, data, name)
{
    .Defunct("oceSetData",
             msg="addColumn() is disallowed and will be removed soon. Use oceSetData() instead. See ?'oce-defunct'.")
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
        ## res <- ctdAddColumn(x, data, name) # FIXME: supply units
        res <- oceSetData(x, name=name, value=data) # FIXME: supply units
    } else {
        res <- x
        res@data[[name]] <- data
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Convert angles from 0:360 to -180:180
#'
#' This is mostly used for instrument heading angles, in cases where the
#' instrument is aligned nearly northward, so that small variations in heading
#' (e.g. due to mooring motion) can yield values that swing from small angles
#' to large angles, because of the modulo-360 cut point.
#' The method is to use the cosine and sine of the angle in order to find "x"
#' and "y" values on a unit circle, and then to use \code{\link{atan2}} to
#' infer the angles.
#'
#' @param theta an angle (in degrees) that is in the range from 0 to 360
#' degrees
#' @return A vector of angles, in the range -180 to 180.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' ## fake some heading data that lie near due-north (0 degrees)
#' n <- 20
#' heading <- 360 + rnorm(n, sd=10)
#' heading <- ifelse(heading > 360, heading - 360, heading)
#' x <- 1:n
#' plot(x, heading, ylim=c(-10, 360), type='l', col='lightgray', lwd=10)
#' lines(x, angleRemap(heading))
angleRemap <- function(theta)
{
    toRad <- atan2(1, 1) / 45
    atan2(sin(toRad * theta), cos(toRad * theta)) / toRad
}


#' Earth magnetic declination
#'
#' Instruments that use magnetic compasses to determine current direction need
#' to have corrections applied for magnetic declination, to get currents with
#' the y component oriented to geographic, not magnetic, north.  Sometimes, and
#' for some instruments, the declination is specified when the instrument is
#' set up, so that the velocities as recorded are already.  Other times, the
#' data need to be adjusted.  This function is for the latter case.
#'
#' @param x an oce object.
#' @param declination magnetic declination (to be added to the heading)
#' @param debug a debugging flag, set to a positive value to get debugging.
#' @return Object, with velocity components adjusted to be aligned with
#' geographic north and east.
#' @author Dan Kelley
#' @seealso Use \code{\link{magneticField}} to determine the declination,
#' inclination and intensity at a given spot on the world, at a given time.
#' @references
#' 1. \samp{https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}
#'
#' @family things related to magnetism
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
        res@data$u <- uvr[1, ]
        res@data$v <- uvr[2, ]
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


#' Trilinear interpolation in a 3D array
#'
#' Interpolate within a 3D array, using the trilinear approximation.
#'
#' Trilinear interpolation is used to interpolate within the \code{f} array,
#' for those (\code{xout}, \code{yout} and \code{zout}) triplets that are
#' inside the region specified by \code{x}, \code{y} and \code{z}.  Triplets
#' that lie outside the range of \code{x}, \code{y} or \code{z} result in
#' \code{NA} values.
#'
#' @param x vector of x values for grid (must be equi-spaced)
#' @param y vector of y values for grid (must be equi-spaced)
#' @param z vector of z values for grid (must be equi-spaced)
#' @param f matrix of rank 3, with the gridd values mapping to the \code{x}
#' values (first index of \code{f}), etc.
#' @param xout vector of x values for output.
#' @param yout vector of y values for output (length must match that of
#' \code{xout}).
#' @param zout vector of z values for output (length must match that of
#' \code{xout}).
#' @return A vector of interpolated values (or \code{NA} values), with length
#' matching that of \code{xout}.
#' @author Dan Kelley and Clark Richards
#'
#' @examples
#' ## set up a grid
#' library(oce)
#' n <- 5
#' x <- seq(0, 1, length.out=n)
#' y <- seq(0, 1, length.out=n)
#' z <- seq(0, 1, length.out=n)
#' f <- array(1:n^3, dim=c(length(x), length(y), length(z)))
#' ## interpolate along a diagonal line
#' m <- 100
#' xout <- seq(0, 1, length.out=m)
#' yout <- seq(0, 1, length.out=m)
#' zout <- seq(0, 1, length.out=m)
#' approx <- approx3d(x, y, z, f, xout, yout, zout)
#' ## graph the results
#' plot(xout, approx, type='l')
#' points(xout[1], f[1, 1, 1])
#' points(xout[m], f[n,n,n])
approx3d <- function(x, y, z, f, xout, yout, zout)
{
    ## Were all arguments given?
    if (missing(x))
        stop("must provide x")
    if (missing(y))
        stop("must provide y")
    if (missing(z))
        stop("must provide z")
    if (missing(f))
        stop("must provide f")
    if (missing(xout))
        stop("must provide xout")
    if (missing(yout))
        stop("must provide yout")
    if (missing(zout))
        stop("must provide zout")
    ## Are there enough data to interpolate?
    if (length(x) < 2)
        stop("must have more than one x value")
    if (length(y) < 2)
        stop("must have more than one y value")
    if (length(x) < 2)
        stop("must have more than one z value")
    ## Are the array dimensions consistent with x, y, and z?
    if (3 != length(dim(f)))
        stop("f must be an array with 3 dimensions")
    if (length(x) != dim(f)[1])
        stop("length of x and dim(f)[1] must agree, but they are ", length(x), " and ", dim(f)[1])
    if (length(y) != dim(f)[2])
        stop("length of y and dim(f)[2] must agree, but they are ", length(y), " and ", dim(f)[2])
    if (length(z) != dim(f)[3])
        stop("length of z and dim(f)[3] must agree, but they are ", length(z), " and ", dim(f)[3])
    ## Are x, y and z equi-spaced?
    if (length(x) > 2) {
        equispaced <- function(a) sd(diff(a)) / mean(diff(a)) < 1e-5
        if (!equispaced(x))
            stop("x values must be equi-spaced")
        if (!equispaced(y))
            stop("y values must be equi-spaced")
        if (!equispaced(z))
            stop("z values must be equi-spaced")
    }
    do_approx3d(x, y, z, f, xout, yout, zout)
}


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

#' Read a World Ocean Atlas NetCDF File
#'
#' @param file character string naming the file
#' @param name of variable to extract. If not provided, an
#' error message is issued that lists the names of data in the file.
#' @param positive logical value indicating whether \code{longitude} should
#' be converted to be in the range from 0 to 360, with \code{name}
#' being shuffled accordingly. This is set to \code{FALSE} by default,
#' because the usual oce convention is for longitude to range between -180
#' to +180.
#'
#' @return A list containing vectors \code{longitude}, \code{latitude},
#' \code{depth}, and an array with the specified name. If \code{positive}
#' is true, then \code{longitude} will be converted to range from 0
#' to 360, and the array will be shuffled accordingly.
#'
#' @examples
#'\dontrun{
#' ## Mean SST at 5-degree spatial resolution
#' tmn <- read.woa("/data/woa13/woa13_decav_t00_5dv2.nc", "t_mn")
#' imagep(tmn$longitude, tmn$latitude, tmn$t_mn[,,1], zlab="SST")
#'}
read.woa <- function(file, name, positive=FALSE)
{
    if (!is.character(file))
        stop("'file' must be a character string")
    con <- ncdf4::nc_open(file)
    if (missing(name)) {
        varnames <- names(con$var)
        stop("must supply a name from the list: ", paste(varnames, collapse=", "))
        return(NULL)
    }
    longitude <- as.vector(ncdf4::ncvar_get(con, "lon"))
    latitude <- as.vector(ncdf4::ncvar_get(con, "lat"))
    depth <- as.vector(ncdf4::ncvar_get(con, "depth"))
    field <- ncdf4::ncvar_get(con, name)
    if (positive) {
        lon2 <- ifelse(longitude < 0, longitude + 360, longitude)
        i  <- order(lon2)
        longitude <- longitude[i]
        ## Crude method to reorder field on first index, whether it is 2D, 3D or 4D,
        ## although I'm not sure that any 4D items occur in the World Ocean Atlas.
        if (is.array(field)) {
            ndim <- length(dim(field))
            if (ndim == 2)
                field <- field[i,]
            else if (ndim == 3)
                field <- field[i,,]
            else if (ndim == 4)
                field <- field[i,,,]
        }
    }
    rval <- list(longitude=longitude, latitude=latitude, depth=depth, field=field)
    names(rval) <- c(head(names(rval), -1), name)
    rval
}

## alphabetized functions END


## unalphabetized functions START

shortenTimeString <- function(t, debug=getOption("oceDebug"))
{
    tc <- as.character(t)
    oceDebug(debug, "shortenTimeString() {\n", sep="", unindent=1)
    oceDebug(debug, "A: '", paste(t, collapse="' '"), "'\n")
    tc <- gsub(" [A-Z]{3}$", "", tc) # remove timezone
    if (all(grepl("^[0-9]{4}", tc))) {
        ## leading years
        years <- substr(tc, 1, 4)
        if (1 == length(unique(years))) {
            tc <- gsub("^[0-9]{4}", "", tc)
            tc <- gsub("^-", "", tc) # works for ISO dates
            oceDebug(debug, "B: '", paste(tc, collapse="' '"), "'\n", sep='')
        }
    } else if (any(grepl("[a-zA-Z]", tc))) {
        ## Change e.g. 'Jul 01' to 'Jul' if all labels end in 01
        if (all(grepl("01\\s*$", tc))) {
            tc <- gsub(" 01\\s*$", "", tc)
            oceDebug(debug, "B: '", paste(tc, collapse="' '"), "'\n", sep='')
        }
    }
    oceDebug(debug, "C: '", paste(tc, collapse="' '"), "'\n", sep='')
    tc <- gsub("^\\s*", "", tc)
    tc <- gsub("\\s*$", "", tc)
    oceDebug(debug, "D: '", paste(tc, collapse="' '"), "'\n", sep='')
    oceDebug(debug, "}\n", unindent=1)
    tc
}

#' Get first finite value in a vector or array, or NULL if none
#' @param v A numerical vector or array.
firstFinite <- function(v)
{
    if (!is.vector(v))
        v <- as.vector(v)
    first <- which(is.finite(v))
    if (length(first) > 0) v[first[1]] else NULL
}

#' Decode units, from strings
#'
#' @param s A string.
#' @return A \code{\link{list}} of two items: \code{unit} which is an
#' \code{\link{expression}}, and \code{scale}, which is a string.
#' @examples
#' unitFromString("DB") # dbar
#' @family functions that interpret variable names and units from headers
unitFromString <- function(s)
{
    ## 1. Strings that have been encountered in WOCE secton (.csv) files
    ## ",,,,,,,,,,,,DBAR,IPTS-68,PSS-78,,PSS-78,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,"
    ##> message("unitFromString(", s, ")")
    if (s == "DB" || s == "DBAR")
        return(list(unit=expression(db), scale=""))
    if (s == "DEG C")
        return(list(unit=expression(degree*C), scale="")) # unknown scale
    if (s == "FMOL/KG")
        return(list(unit=expression(fmol/kg), scale=""))
    if (s == "ITS-90 DEGC" || s == "ITS-90")
        return(list(unit=expression(degree*C), scale="ITS-90"))
    if (s == "IPTS-68 DEGC" || s == "IPTS-68")
        return(list(unit=expression(degree*C), scale="IPTS-68"))
    if (s == "PSS-78")
        return(list(unit=expression(), scale="PSS-78"))
    if (s == "PMOL/KG")
        return(list(unit=expression(pmol/kg), scale=""))
    if (s == "PSU")
        return(list(unit=expression(), scale="PSS-78"))
    if (s == "ML/L")
        return(list(unit=expression(ml/l), scale=""))
    if (s == "UG/L")
        return(list(unit=expression(mu*g/l), scale=""))
    if (s == "UMOL/KG")
        return(list(unit=expression(mu*mol/kg), scale=""))
    if (s == "%")
        return(list(unit=expression(percent), scale=""))
    return(list(unit=as.expression(s), scale=""))
}

## #' Rename a duplicated item (used in reading CTD files)
## #'
## #' Determine a new name for an item that is already in a list of names. This is
## #' done by e.g. appending a \code{2} to the second occurrence of a name, etc.
## #' The purpose is to create distinct variable names for
## #' \code{\link{read.ctd.sbe}}.
## #'
## #' @param existingNames Vector of strings with names already processed.
## #' @param name String with a candidate name.
## #' @return names String with an unduplicated name.
## #' @seealso \code{\link{unduplicateNames}} is similar, but considers
## #' a vector of names.
## #'
## #' @examples
## #' unduplicateName("a", c("a", "b", "a")) # returns "a3"
## unduplicateName <- function(name, existingNames)
## {
##     counter <- 0
##     for (i in seq_along(existingNames)) {
##         if (name == existingNames[i])
##             counter <- counter + 1
##     }
##     res <- if (counter > 0) paste(name, counter+1, sep="") else name
##     ## message("unduplicateName() name: '", name, "'")
##     ## message("         existingNames '", paste(existingNames, collapse="' '"))
##     ## message("         returning '", res, "'")
##     res
## }

#' Rename duplicated character strings
#'
#' Append numeric suffices to character strings, to avoid repeats.
#' This is used by various data
#' input functions, to handle the fact that several oceanographic data
#' formats permit the reuse of variable names within a given file.
#'
#' @param strings Vector of character strings.
#' @param style An integer giving the style. If \code{style}
#' is \code{1}, then e.g. a triplicate of \code{"a"} yields
#' \code{"a"}, \code{"a1"}, and \code{"a2"}.
#' If \code{style} is \code{2}, then the same input yields
#' \code{"a_001"}, \code{"a_002"}, and \code{"a_003"}.
#'
#' @return Vector of strings with repeats distinguished by suffix.
#'
#' @seealso Used by \code{\link{read.ctd.sbe}} with \code{style=1} to
#' rename repeated data elements (e.g. for multiple temperature sensors)
#' in CTD data, and by \code{\link{read.odf}} with \code{style=2} on
#' key-value pairs within ODF metadata.
#'
#' @examples
#' unduplicateNames(c("a", "b", "a", "c", "b"))
#' unduplicateNames(c("a", "b", "a", "c", "b"), style=2)
unduplicateNames <- function(strings, style=1)
{
    ## Handle duplicated names
    if (style == 1) {
        for (i in seq_along(strings)) {
            w <- which(strings == strings[i])
            lw <- length(w)
            if (lw > 1) {
                w <- w[-1]
                strings[w] <- paste(strings[i], 1+seq.int(1, length(w)), sep="")
            }
        }
    } else if (style == 2) {
        for (i in seq_along(strings)) {
            w <- which(strings == strings[i])
            lw <- length(w)
            if (lw > 1) {
                suffices <- seq_len(lw)
                strings[w] <- sprintf("%s_%03d", strings[i], suffices)
            }
        }
    } else {
        stop("unknown style=", style, "; it must be 1 or 2")
    }
    strings
}


#' Rename items in the data slot of an oce object
#'
#' This function may be used to rename elements within the
#' \code{data} slot of \code{oce} objects. It also updates
#' the processing log of the returned object, indicating
#' the changes.
#'
#' @param x An \code{oce} object, i.e. one inheriting from
#' \code{\link{oce-class}}.
#' @param old Vector of strings, containing old names.
#' @param new Vector of strings, containing old names.
#'
#' @examples
#' data(ctd)
#' new <- renameData(ctd, "temperature", "temperature68")
#' new <- oceSetData(new, name="temperature",
#'                   value=T90fromT68(new[["temperature68"]]),
#'                   unit=list(unit=expression(degree*C),scale="ITS=90"))
renameData <- function(x, old=NULL, new=NULL)
{
    if (is.null(old)) stop("need to supply old")
    if (is.null(new)) stop("need to supply new")
    n <- length(old)
    if (n != length(new)) stop("lengths of old and new must match")
    Old <- names(x@data)
    New <- Old
    for (i in 1:n) {
        w <- which(Old == old[i])
        if (length(w) == 0) stop("'", old[i], "' is not in the data slot of x")
        if (length(w) > 1) stop("multiple matches are not permitted")
        ## message("i: ", i, ", old[i]: ", old[i], ", w:", w)
        New[w] <- new[i]
    }
    ## ensure unique ... this is a common user error
    if (length(New) != length(unique(New))) stop("cannot have two columns of same name")
    names(x@data) <- New
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
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
    ## message("in dataLabel()")
    if (!missing(units)) {
        ## message("  dataLabel(); next line is names")
        ## print(names)
        ## message("  dataLabel(); next line is units")
        ## print(units)
        unitsNames <- names(units)
        ##message("  dataLabel(); next line is unitsNames")
        ##print(unitsNames)
        for (i in seq_along(names)) {
            ##message("  i: ", i, ", name: ", names[i])
            w <- which(unitsNames == names[i])
            if (length(w)) {
                ##message("  we match a unit at index w=",  paste(w, collapse=" "))
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

#' Capitalize first letter of each of a vector of words
#'
#' This is used in making labels for data names in some ctd functions
#' @param w vector of character strings
#' @return vector of strings patterned on \code{w} but with first letter
#' in upper case and others in lower case
titleCase <- function(w)
{
    unlist(lapply(seq_along(w),
                  function(i) paste(toupper(substr(w[i], 1, 1)),
                                    tolower(substr(w[i], 2, nchar(w[i]))), sep="")))
}


#' Curl of 2D vector field
#'
#' Calculate the z component of the curl of an x-y vector field.
#'
#' The computed component of the curl is defined by \eqn{\partial }{dv/dx -
#' du/dy}\eqn{ v/\partial x - \partial u/\partial y}{dv/dx - du/dy} and the
#' estimate is made using first-difference approximations to the derivatives.
#' Two methods are provided, selected by the value of \code{method}.
#'
#' \itemize{
#'
#' \item For \code{method=1}, a centred-difference, 5-point stencil is used in
#' the interior of the domain.  For example, \eqn{\partial v/\partial x}{dv/dx}
#' is given by the ratio of \eqn{v_{i+1,j}-v_{i-1,j}}{v[i+1,j]-v[i-1,j]} to the
#' x extent of the grid cell at index \eqn{j}{j}. (The cell extents depend on
#' the value of \code{geographical}.)  Then, the edges are filled in with
#' nearest-neighbour values. Finally, the corners are filled in with the
#' adjacent value along a diagonal.  If \code{geographical=TRUE}, then \code{x}
#' and \code{y} are taken to be longitude and latitude in degrees, and the
#' earth shape is approximated as a sphere with radius 6371km.  The resultant
#' \code{x} and \code{y} are identical to the provided values, and the
#' resultant \code{curl} is a matrix with dimension identical to that of
#' \code{u}.
#'
#' \item For \code{method=2}, each interior cell in the grid is considered
#' individually, with derivatives calculated at the cell center. For example,
#' \eqn{\partial v/\partial x}{dv/dx} is given by the ratio of
#' \eqn{0.5*(v_{i+1,j}+v_{i+1,j+1}) -
#' 0.5*(v_{i,j}+v_{i,j+1})}{0.5*(v[i+1,j]+v[i+1,j+1]) - 0.5*(v[i,j]+v[i,j+1])}
#' to the average of the x extent of the grid cell at indices \eqn{j}{j} and
#' \eqn{j+1}{j+1}. (The cell extents depend on the value of
#' \code{geographical}.)  The returned \code{x} and \code{y} values are the
#' mid-points of the supplied values. Thus, the returned \code{x} and \code{y}
#' are shorter than the supplied values by 1 item, and the returned \code{curl}
#' matrix dimensions are similarly reduced compared with the dimensions of
#' \code{u} and \code{v}.
#' }
#'
#' @param u matrix containing the 'x' component of a vector field
#' @param v matrix containing the 'y' component of a vector field
#' @param x the x values for the matrices, a vector of length equal to the
#' number of rows in \code{u} and \code{v}.
#' @param y the y values for the matrices, a vector of length equal to the
#' number of cols in \code{u} and \code{v}.
#' @param geographical logical value indicating whether \code{x} and \code{y}
#' are longitude and latitude, in which case spherical trigonometry is used.
#' @param method A number indicating the method to be used to calculate the
#' first-difference approximations to the derivatives.  See \dQuote{Details}.
#' @return A list containing vectors \code{x} and \code{y}, along with matrix
#' \code{curl}.  See \dQuote{Details} for the lengths and dimensions, for
#' various values of \code{method}.
#' @section Development status.: This function is under active development as
#' of December 2014 and is unlikely to be stabilized until February 2015.
#' @author Dan Kelley and Chantelle Layton
#' @examples
#' library(oce)
#' ## 1. Shear flow with uniform curl.
#' x <- 1:4
#' y <- 1:10
#' u <- outer(x, y, function(x, y) y/2)
#' v <- outer(x, y, function(x, y) -x/2)
#' C <- curl(u, v, x, y, FALSE)
#'
#' ## 2. Rankine vortex: constant curl inside circle, zero outside
#' rankine <- function(x, y)
#' {
#'     r <- sqrt(x^2 + y^2)
#'     theta <- atan2(y, x)
#'     speed <- ifelse(r < 1, 0.5*r, 0.5/r)
#'     list(u=-speed*sin(theta), v=speed*cos(theta))
#' }
#' x <- seq(-2, 2, length.out=100)
#' y <- seq(-2, 2, length.out=50)
#' u <- outer(x, y, function(x, y) rankine(x, y)$u)
#' v <- outer(x, y, function(x, y) rankine(x, y)$v)
#' C <- curl(u, v, x, y, FALSE)
#' ## plot results
#' par(mfrow=c(2, 2))
#' imagep(x, y, u, zlab="u", asp=1)
#' imagep(x, y, v, zlab="v", asp=1)
#' imagep(x, y, C$curl, zlab="curl", asp=1)
#' hist(C$curl, breaks=100)
#' @family functions relating to vector calculus
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
        res <- do_curl1(u, v, x, y, geographical)
    else if (2 == method)
        res <- do_curl2(u, v, x, y, geographical)
    else
        stop("method must be 1 or 2")
    res
}


#' Calculate Range, Extended a Little, as is Done for Axes
#'
#' This is analogous to what is done as part of the R axis range calculation,
#' in the case where \code{xaxs="r"}.
#'
#' @param x a numeric vector.
#' @param extend fraction to extend on either end
#' @return A two-element vector with the extended range of \code{x}.
#' @author Dan Kelley
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


#' Apply a function to vector data
#'
#' The function \code{FUN} is applied to \code{f} in bins specified by
#' \code{xbreaks}.  (If \code{FUN} is \code{\link{mean}},
#' consider using \code{\link{binMean2D}} instead, since it should be faster.)
#'
#' @param x a vector of numerical values.
#' @param f a vector of data to which the elements of \code{FUN} may be
#' supplied
#' @param xbreaks values of x at the boundaries between bins; calculated using
#' \code{\link{pretty}} if not supplied.
#' @param FUN function to apply to the data
#' @param \dots arguments to pass to the function \code{FUN}
#' @return A list with the following elements: the breaks in x and y
#' (\code{xbreaks} and \code{ybreaks}), the break mid-points (\code{xmids} and
#' \code{ymids}), and a matrix containing the result of applying function
#' \code{FUN} to \code{f} subsetted by these breaks.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' ## salinity profile with median and quartile 1 and 3
#' data(ctd)
#' p <- ctd[["pressure"]]
#' S <- ctd[["salinity"]]
#' q1 <- binApply1D(p, S, pretty(p, 30), function(x) quantile(x, 1/4))
#' q3 <- binApply1D(p, S, pretty(p, 30), function(x) quantile(x, 3/4))
#' plotProfile(ctd, "salinity", col='gray', type='n')
#' polygon(c(q1$result, rev(q3$result)),
#' c(q1$xmids, rev(q1$xmids)), col='gray')
#' points(S, p, pch=20)
#' @family bin-related functions
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
    fSplit <- split(f, cut(x, xbreaks, include.lowest=TRUE, labels=FALSE))
    ##message("length(xbreaks)=", length(xbreaks))
    ##message("length(fSplit)=", length(fSplit))
    result <- unlist(lapply(fSplit, FUN, ...))
    result[!is.finite(result)] <- NA
    names(result) <- NULL
    ## Put some NAs at start and end of 'result', if required because of
    ## 'xbreaks' bins that have no 'x' data.
    xmin <- min(x, na.rm=TRUE)
    xmax <- max(x, na.rm=TRUE)
    nxbreaks <- length(xbreaks)
    if (xmin > xbreaks[2]) {
        m <- which(xbreaks < xmin)[1]
        ##message("condition 1; m=", m)
        result <- c(rep(NA, m), result)
    }
    if (xmax < xbreaks[nxbreaks]) {
        m <- which(xbreaks > xmax)[1]
        ##message("condition 2; m=", m)
        result <- c(result, rep(NA, nxbreaks-m))
    }
    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks), result=result)
}



#' Apply a function to matrix data
#'
#' The function \code{FUN} is applied to \code{f} in bins specified by
#' \code{xbreaks} and \code{ybreaks}.  (If \code{FUN} is \code{\link{mean}},
#' consider using \code{\link{binMean2D}} instead, since it should be faster.)
#'
#' @param x a vector of numerical values.
#' @param y a vector of numerical values.
#' @param f a vector of data to which the elements of \code{FUN} may be
#' supplied
#' @param xbreaks values of x at the boundaries between bins; calculated using
#' \code{\link{pretty}} if not supplied.
#' @param ybreaks values of y at the boundaries between bins; calculated using
#' \code{\link{pretty}} if not supplied.
#' @param FUN function to apply to the data
#' @param \dots arguments to pass to the function \code{FUN}
#' @return A list with the following elements: the breaks in x and y
#' (\code{xbreaks} and \code{ybreaks}), the break mid-points (\code{xmids} and
#' \code{ymids}), and a matrix containing the result of applying function
#' \code{FUN} to \code{f} subsetted by these breaks.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' \dontrun{
#' ## secchi depths in lat and lon bins
#' if (require(ocedata)) {
#'     data(secchi, package="ocedata")
#'     col <- rev(oce.colorsJet(100))[rescale(secchi$depth,
#'                                            xlow=0, xhigh=20,
#'                                            rlow=1, rhigh=100)]
#'     zlim <- c(0, 20)
#'     breaksPalette <- seq(min(zlim), max(zlim), 1)
#'     colPalette <- rev(oce.colorsJet(length(breaksPalette)-1))
#'     drawPalette(zlim, "Secchi Depth", breaksPalette, colPalette)
#'     data(coastlineWorld)
#'     mapPlot(coastlineWorld, longitudelim=c(-5, 20), latitudelim=c(50, 66),
#'       grid=5, fill='gray', projection="+proj=lcc +lat_1=50 +lat_2=65")
#'     bc <- binApply2D(secchi$longitude, secchi$latitude,
#'                      pretty(secchi$longitude, 80),
#'                      pretty(secchi$latitude, 40),
#'                      f=secchi$depth, FUN=mean)
#'     mapImage(bc$xmids, bc$ymids, bc$result, zlim=zlim, col=colPalette)
#'     mapPolygon(coastlineWorld, col='gray')
#' }
#' }
#' @family bin-related functions
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
    A <- split(f, cut(y, ybreaks, labels=FALSE))
    B <- split(x, cut(y, ybreaks, labels=FALSE))
    for (i in seq_along(A)) {
        fSplit <- split(A[[i]], cut(B[[i]], xbreaks, labels=FALSE))
        ##res[, i] <- binApply1D(B[[i]], A[[i]], xbreaks, FUN)$result
        res[, i] <- unlist(lapply(fSplit, FUN, ...))
    }
    res[!is.finite(res)] <- NA
    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks),
         ybreaks=ybreaks, ymids=ybreaks[-1]-0.5*diff(ybreaks),
         result=res)
}


#' Bin-count vector data
#'
#' Count the number of elements of a given vector that fall within
#' successive pairs of values within a second vector.
#'
#' @param x Vector of numerical values.
#' @param xbreaks Vector of values of x at the boundaries between bins, calculated using
#' \code{\link{pretty}} if not supplied.
#' @return A list with the following elements: the breaks (\code{xbreaks},
#' midpoints (\code{xmids}) between those breaks, and
#' the count (\code{number}) of \code{x} values between successive breaks.
#' @author Dan Kelley
#' @family bin-related functions
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

#' Bin-average f=f(x)
#'
#' Average the values of a vector \code{f} in bins defined on another
#' vector \code{x}. A common example might be averaging CTD profile
#' data into pressure bins (see \dQuote{Examples}).
#'
#' @param x Vector of numerical values.
#' @param f Vector of numerical values.
#' @param xbreaks Vector of values of x at the boundaries between bins, calculated using
#' \code{\link{pretty}} if not supplied.
#' @return A list with the following elements: the breaks (\code{xbreaks},
#' midpoints (\code{xmids}) between those breaks,
#' the count (\code{number}) of \code{x} values between successive breaks,
#' and the resultant average (\code{result}) of \code{f}, classified by the
#' \code{x} breaks.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' z <- ctd[["z"]]
#' T <- ctd[["temperature"]]
#' plot(T, z)
#' TT <- binMean1D(z, T, seq(-100, 0, 1))
#' lines(TT$result, TT$xmids, col='red')
#'
#' @author Dan Kelley
#' @family bin-related functions
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

#' Bin-count matrix data
#'
#' Count the number of elements of a given matrix z=z(x,y) that fall within
#' successive pairs of breaks in x and y.
#'
#' @param x Vector of numerical values.
#' @param y Vector of numerical values.
#' @param xbreaks Vector of values of \code{x} at the boundaries between bins, calculated using
#' \code{\link{pretty}(x)} if not supplied.
#' @param ybreaks Vector of values of \code{y} at the boundaries between bins, calculated using
#' \code{\link{pretty}(y)} if not supplied.
#' @param flatten A logical value indicating whether
#' the return value also contains equilength
#' vectors \code{x}, \code{y}, \code{z} and \code{n}, a flattened
#' representation of \code{xmids}, \code{ymids}, \code{result} and
#' \code{number}.
#' @return A list with the following elements: the breaks (\code{xbreaks}
#' and \code{ybreaks}), the midpoints (\code{xmids} and \code{ymids})
#' between those breaks, and
#' the count (\code{number}) of \code{f} values in the boxes defined
#' between successive breaks.
#' @author Dan Kelley
#' @family bin-related functions
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
            number=integer( (nxbreaks-1) * (nybreaks-1) ),
            mean=double( (nxbreaks-1) * (nybreaks-1) ),
            NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
                ybreaks=ybreaks,
                xmids=xbreaks[-1] - 0.5 * diff(xbreaks),
                ymids=ybreaks[-1] - 0.5 * diff(ybreaks),
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


#' Bin-average f=f(x,y)
#'
#' Average the values of a vector \code{f(x,y)} in bins defined on
#' vectors \code{x} and \code{y}. A common example might be averaging
#' spatial data into location bins.
#'
#' @param x Vector of numerical values.
#' @param y Vector of numerical values.
#' @param f Matrix of numerical values, a matrix f=f(x,y).
#' @param xbreaks Vector of values of \code{x} at the boundaries between bins, calculated using
#' \code{\link{pretty}(x)} if not supplied.
#' @param ybreaks Vector of values of \code{y} at the boundaries between bins, calculated using
#' \code{\link{pretty}(y)} if not supplied.
#' @param flatten A logical value indicating whether
#' the return value also contains equilength
#' vectors \code{x}, \code{y}, \code{z} and \code{n}, a flattened
#' representation of \code{xmids}, \code{ymids}, \code{result} and
#' \code{number}.
#' @param fill Logical value indicating whether to fill \code{NA}-value gaps in
#' the matrix. Gaps will be filled as the average of linear interpolations
#' across rows and columns. See \code{fillgap}, which works together with this.
#' @param fillgap Integer controlling the size of gap that can be filled
#' across. If this is negative (as in the default), gaps will be filled
#' regardless of their size. If it is positive, then gaps exceeding this
#' number of indices will not be filled.
#'
#' @return A list with the following elements: the midpoints (renamed as
#' \code{x} and \code{y}), the count (\code{number}) of \code{f(x,y)} values
#' for \code{x} and \code{y} values that lie between corresponding breaks,
#' and the resultant average (\code{f}) of \code{f(x,y)}, classified by the
#' \code{x} and \code{y} breaks.
#'
#' @examples
#' library(oce)
#' x <- runif(500)
#' y <- runif(500)
#' f <- x + y
#' xb <- seq(0, 1, 0.1)
#' yb <- seq(0, 1, 0.2)
#' m <- binMean2D(x, y, f, xb, yb)
#' plot(x, y)
#' contour(m$xmids, m$ymids, m$result, add=TRUE, levels=seq(0, 2, 0.5), labcex=1)
#'
#' @author Dan Kelley
#' @family bin-related functions
binMean2D <- function(x, y, f, xbreaks, ybreaks, flatten=FALSE, fill=FALSE, fillgap=-1)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (fillgap == 0) stop("cannot have a negative 'fillgap' value")
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
            as.integer(fill), as.integer(fillgap),
            number=integer( (nxbreaks-1) * (nybreaks-1) ),
            mean=double( (nxbreaks-1) * (nybreaks-1) ),
            NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
                 ybreaks=ybreaks,
                 xmids=xbreaks[-1] - 0.5 * diff(xbreaks),
                 ymids=ybreaks[-1] - 0.5 * diff(ybreaks),
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


#' Bin-average a vector y, based on x values
#'
#' The \code{y} vector is averaged in bins defined for \code{x}.  Missing
#' values in \code{y} are ignored.
#'
#' @param x a vector of numerical values.
#' @param y a vector of numerical values.
#' @param xmin x value at the lower limit of first bin; the minimum \code{x}
#' will be used if this is not provided.
#' @param xmax x value at the upper limit of last bin; the maximum \code{x}
#' will be used if this is not provided.
#' @param xinc width of bins, in terms of x value; 1/10th of \code{xmax-xmin}
#' will be used if this is not provided.
#' @return A list with two elements: \code{x}, the mid-points of the bins, and
#' \code{y}, the average \code{y} value in the bins.
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' ## A. fake linear data
#' x <- seq(0, 100, 1)
#' y <- 1 + 2 * x
#' plot(x, y, pch=1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch=3, col='red', cex=3)
#'
#' ## B. fake quadratic data
#' y <- 1 + x ^2
#' plot(x, y, pch=1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch=3, col='red', cex=3)
#'
#' ## C. natural data
#' data(co2)
#' plot(co2)
#' avg <- binAverage(time(co2), co2, 1950, 2000, 2)
#' points(avg$x, avg$y, col='red')
#' @family bin-related functions
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



#' Extract (x, y, z) from (x, y, grid)
#'
#' Extract the grid points from a grid, returning columns.
#' This is useful for e.g. gridding large datasets, in which the first step
#' might be to use \code{\link{binMean2D}}, followed by
#' \code{\link{interpBarnes}}.
#'
#' @param x a vector holding the x coordinates of grid.
#' @param y a vector holding the y coordinates of grid.
#' @param grid a matrix holding the grid.
#' @return A list containing three vectors: \code{x}, the grid x values,
#' \code{y}, the grid y values, and \code{grid}, the grid values.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(wind)
#' u <- interpBarnes(wind$x, wind$y, wind$z)
#' contour(u$xg, u$yg, u$zg)
#' U <- ungrid(u$xg, u$yg, u$zg)
#' points(U$x, U$y, col=oce.colorsJet(100)[rescale(U$grid, rlow=1, rhigh=100)], pch=20)
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


#' Draw error bars on an existing xy diagram
#'
#' @param x x coordinates of points on the existing plot.
#' @param y y coordinates of points on the existing plot.
#' @param xe error on x coordinates of points on the existing plot, either a
#' single number or a vector of length identical to that of \code{y}.
#' @param ye as \code{xe} but for y coordinate.
#' @param percent boolean flag indicating whether \code{xe} and \code{ye} are
#' in terms of percent of the corresponding \code{x} and \code{y} values.
#' @param style indication of the style of error bar.  Using \code{style=0}
#' yields simple line segments (drawn with \code{\link{segments}}) and
#' \code{style=1} yields line segments with short perpendicular endcaps.
#' @param length length of endcaps, for \code{style=1} only; it is passed to
#' \code{\link{arrows}}, which is used to draw that style of error bars.
#' @param \dots graphical parameters passed to the code that produces the error
#' bars, e.g. to \code{\link{segments}} for \code{style=0}.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(ctd)
#' S <- ctd[["salinity"]]
#' T <- ctd[["temperature"]]
#' plot(S, T)
#' errorbars(S, T, 0.05, 0.5)
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


#' Find indices of times in an ordered vector [defunct]
#'
#' \strong{WARNING:} This function will be removed soon; see \link{oce-defunct}.
#'
#' @param x Ignored, since this function is defunct.
#' @param f Ignored, since this function is defunct.
#' @author Dan Kelley
#' @family functions that will be removed soon
findInOrdered <- function(x, f)
{
    .Defunct("findInterval",
             msg="findInOrdered() is disallowed and will be removed soon. Use findInterval() instead. See ?'oce-defunct'.")
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


#' Plot a Model-data Comparison Diagram
#'
#' Creates a diagram as described by Taylor (2001).  The graph is in the form
#' of a semicircle, with radial lines and spokes connecting at a focus point on
#' the flat (lower) edge.  The radius of a point on the graph indicates the
#' standard deviation of the corresponding quantity, i.e. x and the columns in
#' y.  The angle connecting a point on the graph to the focus provides an
#' indication of correlation coefficient with respect to x.  The ``east'' side
#' of the graph indicates \eqn{R=1}{R=1}, while \eqn{R=0}{R=0} is at the
#' ``north edge'' and \eqn{R=-1}{R=-1} is at the ``west'' side.  The \code{x}
#' data are indicated with a bullet on the graph, appearing on the lower edge
#' to the right of the focus at a distance indicating the standard deviation of
#' \code{x}.  The other points on the graph represent the columns of \code{y},
#' coded automatically or with the supplied values of \code{pch} and
#' \code{col}.
#' The example shows two tidal models of the Halifax sealevel data, computed
#' with \code{\link{tidem}} with just the M2 component and the S2 component;
#' the graph indicates that the M2 model is much better than the S2 model.
#'
#' @param x a vector of reference values of some quantity, e.g. measured over
#' time or space.
#' @param y a matrix whose columns hold values of values to be compared with
#' those in x.  (If \code{y} is a vector, it is converted first to a one-column
#' matrix).
#' @param scale optional scale, interpreted as the maximum value of standard
#' deviation.
#' @param pch list of characters to plot, one for each column of \code{y}.
#' @param col list of colors for points on the plot, one for each column of
#' \code{y}.
#' @param labels optional vector of strings to use for labelling the points.
#' @param pos optional vector of positions for labelling strings.  If not
#' provided, labels will be to the left of the symbols.
#' @param \dots optional arguments passed by \code{plotTaylor} to more child
#' functions.
#' @author Dan Kelley
#' @references Taylor, Karl E., 2001.  Summarizing multiple aspects of model
#' performance in a single diagram, \emph{J. Geophys. Res.}, 106:D7,
#' 7183--7192.
#' @examples
#'
#' library(oce)
#' data(sealevel)
#' x <- sealevel[["elevation"]]
#' M2 <- predict(tidem(sealevel, constituents="M2"))
#' S2 <- predict(tidem(sealevel, constituents=c("S2")))
#' plotTaylor(x, cbind(M2, S2))
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
    for (column in seq_len(ncol(y))) {
        ySD <- sd(y[, column], na.rm=TRUE)
        R <- cor(x, y[, column])^2
        ##cat("column=", column, "ySD=", ySD, "R=", R, "col=", col[column], "pch=", pch[column], "\n")
        xx <- ySD * cos( (1 - R) * pi / 2 )
        yy <- ySD * sin( (1 - R) * pi / 2 )
        points(xx, yy, pch=pch[column], lwd=2, col=col[column], cex=2)
        if (haveLabels) {
            ##cat(labels[column], "at", pos[column], "\n")
            text(xx, yy, labels[column], pos=pos[column], ...)
        }
    }
}


#' Pretty lat/lon in deg, min, sec
#'
#' Round a geographical positions in degrees, minutes, and seconds
#' Depending on the range of values in \code{x}, rounding is done to degrees,
#' half-degrees, minutes, etc.
#'
#' @param x a series of one or more values of a latitude or longitude, in
#' decimal degrees
#' @param debug set to a positive value to get debugging information during
#' processing.
#' @return A vector of numbers that will yield good axis labels if
#' \code{\link{formatPosition}} is used.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' formatPosition(prettyPosition(10+1:10/60+2.8/3600))
prettyPosition <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "prettyPosition(...) {\n", sep="", unindent=1)
    r <- diff(range(x, na.rm=TRUE))
    oceDebug(debug, 'range(x)=', range(x), 'r=', r, '\n')
    if (r > 5) {
        ## D only
        res <- pretty(x)
    } else if (r > 1) {
        ## round to 30 minutes
        res <- (1 / 2) * pretty(2 * x)
        oceDebug(debug, "case 1: res=", res, "\n")
    } else if (r > 30/60) {
        ## round to 1 minute, with extras
        res <- (1 / 60) * pretty(60 * x, n=6)
        oceDebug("case 2: res=", res, "\n")
    } else if (r > 5/60) {
        ## round to 1 minute
        res <- (1 / 60) * pretty(60 * x, 4)
        oceDebug(debug, "case 3: res=", res, "\n")
    } else if (r > 10/3600) {
        ## round to 10 sec
        res <- (1 / 360) * pretty(360 * x)
        oceDebug(debug, "case 4: res=", res, "\n")
    } else {
        ## round to seconds
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


#' Rescale values to lie in a given range
#'
#' This is helpful in e.g. developing a color scale for an image plot.  It is
#' not necessary that \code{rlow} be less than \code{rhigh}, and in fact
#' reversing them is a good way to get a reversed color scale for a plot.
#'
#' @param x a numeric vector.
#' @param xlow \code{x} value to correspond to \code{rlow}.  If not given, it
#' will be calculated as the minimum value of \code{x}
#' @param xhigh \code{x} value to correspond to \code{rhigh}.  If not given, it
#' will be calculated as the maximum value of \code{x}
#' @param rlow value of the result corresponding to \code{x} equal to
#' \code{xlow}.
#' @param rhigh value of the result corresponding to \code{x} equal to
#' \code{xhigh}.
#' @param clip logical, set to \code{TRUE} to clip the result to the range
#' spanned by \code{rlow} and \code{rhigh}.
#' @return A new vector, which has minimum \code{lim[1]} and maximum
#' \code{lim[2]}.
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' # Fake tow-yow data
#' t <- seq(0, 600, 5)
#' x <- 0.5 * t
#' z <- 50 * (-1 + sin(2 * pi * t / 360))
#' T <- 5 + 10 * exp(z / 100)
#' palette <- oce.colorsJet(100)
#' zlim <- range(T)
#' drawPalette(zlim=zlim, col=palette)
#' plot(x, z, type='p', pch=20, cex=3,
#'      col=palette[rescale(T, xlow=zlim[1], xhigh=zlim[2], rlow=1, rhigh=100)])
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


#' Adjust the time within Oce object
#'
#' This function compensates for drifting instrument clocks, according to
#' \eqn{t'=t + a + b (t-t0)}{t'=t + a + b*(t-t0)}, where \eqn{t'}{t'} is the
#' true time and \eqn{t}{t} is the time stored in the object.  A single check
#' on time mismatch can be described by a simple time offset, with a non-zero
#' value of \code{a}, a zero value of \code{b}, and an arbitrary value of
#' \code{t0}.  Checking the mismatch before and after an experiment yields
#' sufficient information to specify a linear drift, via \code{a}, \code{b},
#' and \code{t0}.  Note that \code{t0} is just a convenience parameter, which
#' avoids the user having to know the "zero time" used in R and clarifies the
#' values of the other two parameters.  It makes sense for \code{t0} to have
#' the same timezone as the time within \code{x}.
#'
#' The returned object is computed by linear interpolation, using
#' \code{\link{approx}} with \code{rule=2}, to avoid \code{NA} values at the
#' start or end.  The new time will be as given by the formula above. Note that
#' if the drift is large enough, the sampling rate will be changed.  It is a
#' good idea to start with an object that has an extended time range, so that,
#' after this is called, \code{\link{subset}} can be used to trim to a desired
#' time range.
#'
#' @param x an \code{oce} object (presently, this must be of class \code{adv})
#' @param a intercept [in seconds] in linear model of time drift (see
#' \dQuote{Details}).
#' @param b slope [unitless] in linear model of time drift [unitless] (see
#' \dQuote{Details}).
#' @param t0 reference time [in POSIXct format] used in linear model of time
#' drift (see \dQuote{Details}).
#' @param debug a flag that, if nonzero, turns on debugging.
#' @return A new object, with time and other data adjusted.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(adv)
#' adv2 <- retime(adv,0,1e-4,as.POSIXct("2008-07-01 00:00:00",tz="UTC"))
#' plot(adv[["time"]], adv2[["time"]]-adv[["time"]], type='l')
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
    oceDebug(debug, paste("retime.adv(x, a=", a, ", b=", b, ", t0=\"", format(t0), "\")\n"), sep="", unindent=1)
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


#' Calculate min, mean, and max values
#'
#' This is a faster cousin of the standard \code{\link{fivenum}} function,
#' used in generic \code{summary} functions for \code{oce} objects.
#'
#' @param x a vector or matrix of numerical values.
#' @return A character vector of four values: the minimum, the mean, the
#' maximum, and an indication of the number of data.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' threenum(1:10)
threenum <- function(x)
{
    dim <- dim(x)
    if (is.character(x) || is.null(x)) {
        res <- rep(NA, 3)
    } else if (is.list(x)) {
        if (2 == sum(c("lsb", "msb") %in% names(x))) {
            ## e.g. landsat data
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
    } else if (0 < sum(!is.na(x))) {
        res <- c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
    } else {
        res <- rep(NA, 3)
    }
    ## 20160314: tack on dimensions, neccessitating conversion to character
    ##res <- format(res, digits=4)
    r1 <- format(res[1], digits=5)
    r2 <- format(res[2], digits=5)
    r3 <- format(res[3], digits=5)
    res <- c(r1, r2, r3)
    ## res[1] <- format(res[1], digits=4) # do these independently
    ## res[2] <- format(res[2], digits=4)
    ## res[3] <- format(res[3], digits=4)
    if (is.array(x)) {
        res <- c(res, paste(dim(x), collapse="x"))
    } else {
        res <- c(res, format(length(x)))
    }
    res
}

normalize <- function(x)
{
    var <- var(x, na.rm=TRUE)
    if (var == 0)
        rep(0, length(x))
    else
        (x - mean(x, na.rm=TRUE)) / sqrt(var)
}


#' Detrend a set of observations
#'
#' Detrends \code{y} by subtracting a linear trend in \code{x}, to create
#' a vector that is zero for its first and last finite value.
#' If the second parameter (\code{y}) is missing, then \code{x} is
#' taken to be \code{y}, and a new \code{x} is constructed with
#' \code{\link{seq_along}}.  Any \code{NA} values are left as-is.
#'
#' A common application is to bring the end points of a time series
#' down to zero, prior to applying a digital filter. (See examples.)
#'
#' @param x a vector of numerical values.  If \code{y} is not given, then
#' \code{x} is taken for \code{y}.
#' @param y an optional vector
#' @return A list containing \code{Y}, the detrended version of \code{y}, and
#' the intercept \code{a} and slope \code{b} of the linear function of \code{x}
#' that is subtracted from \code{y} to yield \code{Y}.
#' @author Dan Kelley
#' @examples
#'
#' x <- seq(0, 0.9 * pi, length.out=50)
#' y <- sin(x)
#' y[1] <- NA
#' y[10] <- NA
#' plot(x, y, ylim=c(0, 1))
#' d <- detrend(x, y)
#' points(x, d$Y, pch=20)
#' abline(d$a, d$b, col='blue')
#' abline(h=0)
#' points(x, d$Y + d$a + d$b * x, col='blue', pch='+')
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
    first <- which(is.finite(y))[1]
    last <- 1 + length(y) - which(is.finite(rev(y)))[1]
    if (x[first] == x[last])
        stop("the first and last x values must be distinct")
    b <- (y[first] - y[[last]]) / (x[first] - x[[last]])
    a <- y[first] - b * x[first]
    list(Y=y - (a+b*x), a=a, b=b)
}



#' Remove spikes from a time series
#'
#' The method identifies spikes with respect to a "reference" time-series, and
#' replaces these spikes with the reference value, or with \code{NA} according
#' to the value of \code{action}; see \dQuote{Details}.
#'
#' @details
#' Three modes of operation are permitted, depending on the value of
#' \code{reference}.
#'
#'\itemize{
#'
#'\item For \code{reference="median"}, the first step is to linearly interpolate
#' across any gaps (spots where \code{x==NA}), using \code{\link{approx}} with
#' \code{rule=2}. The second step is to pass this through
#' \code{\link{runmed}} to get a running median spanning \code{k}
#' elements. The result of these two steps is the "reference" time-series.
#' Then, the standard deviation of the difference between \code{x}
#' and the reference is calculated.  Any \code{x} values that differ from
#' the reference by more than \code{n} times this standard deviation are considered
#' to be spikes.  If \code{replace="reference"}, the spike values are
#' replaced with the reference, and the resultant time series is
#' returned.  If \code{replace="NA"}, the spikes are replaced with \code{NA},
#' and that result is returned.
#'
#'\item For \code{reference="smooth"}, the processing is the same as for
#' \code{"median"}, except that \code{\link{smooth}} is used to calculate the
#' reference time series.
#'
#'\item For \code{reference="trim"}, the reference time series is constructed by
#' linear interpolation across any regions in which \code{x<min} or
#' \code{x>max}.  (Again, this is done with \code{\link{approx}} with
#' \code{rule=2}.) In this case, the value of \code{n} is ignored, and the
#' return value is the same as \code{x}, except that spikes are replaced
#' with the reference series (if \code{replace="reference"} or with
#' \code{NA}, if \code{replace="NA"}.
#'}
#'
#' @param x a vector of (time-series) values, a list of vectors, a data frame,
#' or an object that inherits from class \code{oce}.
#' @param reference indication of the type of reference time series to be used
#' in the detection of spikes; see \sQuote{Details}.
#' @param n an indication of the limit to differences between \code{x} and the
#' reference time series, used for \code{reference="median"} or
#' \code{reference="smooth"}; see \sQuote{Details.}
#' @param k length of running median used with \code{reference="median"}, and
#' ignored for other values of \code{reference}.
#' @param min minimum non-spike value of \code{x}, used with
#' \code{reference="trim"}.
#' @param max maximum non-spike value of \code{x}, used with
#' \code{reference="trim"}.
#' @param replace an indication of what to do with spike values, with
#' \code{"reference"} indicating to replace them with the reference time
#' series, and \code{"NA"} indicating to replace them with \code{NA}.
#' @param skip optional vector naming columns to be skipped. This is ignored if
#' \code{x} is a simple vector. Any items named in \code{skip} will be passed
#' through to the return value without modification.  In some cases,
#' \code{despike} will set up reasonable defaults for \code{skip}, e.g. for a
#' \code{ctd} object, \code{skip} will be set to \code{c("time", "scan",
#' "pressure")} if it is not supplied as an argument.
#' @return A new vector in which spikes are replaced as described above.
#' @author Dan Kelley
#' @examples
#'
#' n <- 50
#' x <- 1:n
#' y <- rnorm(n=n)
#' y[n/2] <- 10                    # 10 standard deviations
#' plot(x, y, type='l')
#' lines(x, despike(y), col='red')
#' lines(x, despike(y, reference="smooth"), col='darkgreen')
#' lines(x, despike(y, reference="trim", min=-3, max=3), col='blue')
#' legend("topright", lwd=1, col=c("black", "red", "darkgreen", "blue"),
#'        legend=c("raw", "median", "smooth", "trim"))
#'
#' # add a spike to a CTD object
#' data(ctd)
#' plot(ctd)
#' T <- ctd[["temperature"]]
#' T[10] <- T[10] + 10
#' ctd[["temperature"]] <- T
#' CTD <- despike(ctd)
#' plot(CTD)
despike <- function(x, reference=c("median", "smooth", "trim"), n=4, k=7, min=NA, max=NA,
                    replace=c("reference", "NA"), skip)
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
                    ## check for NA column
                    if (all(is.na(x[[column]]))) {
                        warning(paste("Column", column, "contains only NAs. Skipping"))
                    } else {
                        x[[column]] <- despikeColumn(x[[column]],
                                                     reference=reference, n=n, k=k, min=min, max=max, replace=replace)
                    }
                }
            }
            x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        } else {
            columns <- names(x)
            for (column in columns) {
                if (!(column %in% skip)) {
                    if (all(is.na(x[[column]]))) {
                        warning(paste("Column", column, "contains only NAs. Skipping"))
                    } else {
                        x[[column]] <- despikeColumn(x[[column]],
                                                     reference=reference, n=n, k=k, min=min, max=max, replace=replace)
                    }
                }
            }
        }
    }
    x
}

despikeColumn <- function(x, reference=c("median", "smooth", "trim"), n=4, k=7, min=NA, max=NA,
                          replace=c("reference", "NA"))
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
        x.gapless <- approx(i[!na], x[!na], i, rule=2)$y
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
            if (replace == "reference") {
                x[bad] <- approx(i[!bad], x.gapless[!bad], i[bad], rule=2)$y
            } else {
                x[bad] <- NA
            }
        }
    } else {
        stop("unknown reference ", reference)
    }
    x
}



#' Substitute NA for data outside a range
#'
#' Substitute NA for data outside a range, e.g. to remove wild spikes in data.
#'
#' @param x vector of values
#' @param min minimum acceptable value.  If not supplied, and if \code{max} is
#' also not supplied, a \code{min} of the 0.5 percentile will be used.
#' @param max maximum acceptable value.  If not supplied, and if \code{min} is
#' also not supplied, a \code{min} of the 0.995 percentile will be used.
#' @author Dan Kelley
#' @examples
#'
#' ten.to.twenty <- rangeLimit(1:100, 10, 20)
rangeLimit <- function(x, min, max)
{
    if (missing(min) && missing(max)) {
        minmax <- quantile(x, 0.005, 0.995)
        min <- minmax[1]
        max <- minmax[2]
    }
    ifelse(max < x | x < min, NA, x)
}


#' Determine year from various abbreviations
#'
#' Various data files may contain various abbreviations for years.  For
#' example, 99 refers to 1999, and 8 refers to 2008.  Sometimes, even 108
#' refers to 2008 (the idea being that the "zero" year was 1900).  This
#' function deals with the three cases mentioned.  It will fail if someone
#' supplies 60, meaning year 2060 as opposed to 1960.
#'
#' @param year a year, or vector of years, possibly abbreviated
#' @author Dan Kelley
#' @examples
#'
#' fullYear <- unabbreviateYear(c(99, 8, 108))
#' @family things related to time
unabbreviateYear <- function(year)
{
    ## handle e.g. 2008 as 2008 (full year), 8 (year-2000 offset), or 108 (year 1900 offset)
    ##cat("year[1]=",year[1])
    ##res <- ifelse(year > 1800, year, ifelse(year > 100, year + 1900, year + 2000))
    ##cat(" became ", res[1], "\n")
    ##res
    ifelse(year > 1800, year, ifelse(year > 50, year + 1900, year + 2000))
}



#' Unwrap an angle that suffers modulo-360 problems
#'
#' This is mostly used for instrument heading angles, in cases where the
#' instrument is aligned nearly northward, so that small variations in heading
#' (e.g. due to mooring motion) can yield values that swing from small angles
#' to large angles, because of the modulo-360 cut point.
#' The method is to use the cosine and sine of the angle, to construct "x" and
#' "y" values on a unit circle, then to find means and medians of x and y
#' respectively, and finally to use \code{\link{atan2}} to infer the angles.
#'
#' @param angle an angle (in degrees) that is thought be near 360 degrees, with
#' added noise
#' @return A list with two estimates: \code{mean} is based on an arithmetic
#' mean, and \code{median} is based on the median. Both are mapped to the range
#' 0 to 360.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' true <- 355
#' a <- true + rnorm(100, sd=10)
#' a <- ifelse(a > 360, a - 360, a)
#' a2 <- unwrapAngle(a)
#' par(mar=c(3, 3, 5, 3))
#' hist(a, breaks=360)
#' abline(v=a2$mean, col="blue", lty="dashed")
#' abline(v=true, col="blue")
#' mtext("true (solid)\n estimate (dashed)", at=true, side=3, col="blue")
#' abline(v=mean(a), col="red")
#' mtext("mean", at=mean(a), side=3, col="red")
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


#' Partial matching of strings or numbers
#'
#' An extended version of \code{\link{pmatch}} that allows \code{x} to be
#' numeric or string-based.  As with \code{\link{pmatch}}, partial string
#' matches are handled.
#' This is a wrapper that is useful mainly for \code{which} arguments to
#' plotting functions.
#'
#' @aliases oce.pmatch
#' @param x a code, or vector of codes.  This may be numeric, in which case it
#' is simply returned without further analysis of the other arguments, or it
#' may be string-based, in which case \code{\link{pmatch}} is used to find
#' numeric matches.
#' @param table a list that maps strings to numbers; \code{\link{pmatch}} is
#' used on \code{names(table)}.  If the name contains characters that are
#' normally not permitted in a variable name, use quotes, e.g.
#' \code{list(salinity=1, temperature=2, "salinity+temperature"=3)}.
#' @param nomatch value to be returned for cases of no match (passed to
#' \code{\link{pmatch}}.
#' @param duplicates.ok code for the handling of duplicates (passed to
#' \code{\link{pmatch}}).
#' @return A number, or vector of numbers, corresponding to the matches.
#' Non-matches are indicated with \code{NA} values, or whatever value is given
#' by the \code{NA} argument.
#' @author Dan Kelley
#' @seealso Since \code{\link{pmatch}} is used for the actual matching, its
#' documentation should be consulted.
#' @examples
#'
#' library(oce)
#' oce.pmatch(c("s", "at", "te"), list(salinity=1, temperature=3.1))
ocePmatch <- function(x, table, nomatch=NA_integer_, duplicates.ok=FALSE)
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
oce.pmatch <- ocePmatch


#' Wrapper to give normalized spectrum
#'
#' This is a wrapper around the R \code{\link{spectrum}} function, which
#' returns spectral values that are adjusted so that the integral of those
#' values equals the variance of the input \code{x}.
#'
#' @aliases oce.spectrum oceSpectrum
#' @param x As for \code{\link{spectrum}}, a univariate or multivariate time
#' series.
#' @param \dots extra arguments passed on to \code{\link{spectrum}}.
#' @return A spectrum that has values that integrate to the variance.
#' @author Dan Kelley
#' @seealso \code{\link{spectrum}}.
#' @examples
#'   x <- rnorm(1e3)
#'   s <- spectrum(x, plot=FALSE)
#'   ss <- oce.spectrum(x, plot=FALSE)
#'   cat("variance of x=", var(x), "\n")
#'   cat("integral of     spectrum=", sum(s$spec)*diff(s$freq[1:2]), "\n")
#'   cat("integral of oce.spectrum=", sum(ss$spec)*diff(ss$freq[1:2]), "\n")
oceSpectrum <- function(x, ...)
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
oce.spectrum <- oceSpectrum


#' Show some values from a vector
#'
#' This is similar to \code{\link{str}}, but it shows data at the first and
#' last of the vector, which can be quite helpful in debugging.
#'
#' @param v the vector.
#' @param msg a message to show, introducing the vector.  If not provided, then
#' a message is created from \code{v}.
#' @param digits for numerical values of \code{v}, this is the number of digits
#' to use, in formatting the numbers with \code{\link{format}}; otherwise,
#' \code{digits} is ignored.
#' @param n number of elements to at start and end. If \code{n}
#' is negative, then all the elements are shown.
#' @return A string, suitable for using in \code{\link{cat}} or
#' \code{\link{oceDebug}}.
#' @author Dan Kelley
vectorShow <- function(v, msg, digits=5, n=2L)
{
    nv <- length(v)
    if (missing(msg))
        msg <- deparse(substitute(v))
    if (nv == 0) {
        paste(msg, "(empty vector)\n")
    } else {
        if (n < 0 || nv <= 2*n) {
            showAll <- TRUE
        } else {
            n <- floor(min(n, nv/2))
            showAll <- FALSE
        }
        if (is.numeric(v)) {
            if (showAll) {
                paste(msg, ": ", paste(format(v, digits=digits), collapse=", "),
                      " (length ", nv, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(format(v[1:n], digits=digits), collapse=", "),
                      ", ..., ", paste(format(v[nv-seq.int(n-1, 0)], digits=digits), collapse=", "),
                      " (length ", nv, ")\n", sep="")
            }
        } else {
            if (showAll) {
                paste(msg, ": ", paste(v, collapse=", "),
                      " (length ", nv, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(v[1:n], collapse=", "),
                      ", ..., ", paste(v[nv-seq.int(n-1, 0)], collapse=", "),
                      " (length ", nv, ")\n", sep="")
            }
        }
    }
}


#' Full name of file, including path
#'
#' Determines the full name of a file, including the path.  Used by many
#' \code{read.X} routines, where \code{X} is the name of a class of object.
#' This is a wrapper around \code{\link{normalizePath}}, with warnings turned
#' off so that messages are not printed for unfound files (e.g. URLs).
#'
#' @param filename name of file
#' @return Full file name
#' @author Dan Kelley
fullFilename <- function(filename)
{
    warn <- options('warn')$warn
    options(warn=-1)
    res <- normalizePath(filename)
    options(warn=warn)
    res
}


#' Provide axis names in adjustable sizes
#'
#' Provide axis names in adjustable sizes, e.g. using T instead of Temperature,
#' and including units as appropriate.
#' Used by e.g. \code{\link{plot,ctd-method}}.
#'
#' @param item code for the label.  This must be an element from the following
#' list, or an abbreviation that uniquely identifies an element through its
#' first letters: \code{"S"}, \code{"C"}, \code{"conductivity mS/cm"},
#' \code{"conductivity S/m"}, \code{"T"}, \code{"theta"}, \code{"sigmaTheta"},
#' \code{"conservative temperature"}, \code{"absolute salinity"},
#' \code{"nitrate"}, \code{"nitrite"}, \code{"oxygen"}, \code{"oxygen
#' saturation"}, \code{"oxygen mL/L"}, \code{"oxygen umol/L"}, \code{"oxygen
#' umol/kg"}, \code{"phosphate"}, \code{"silicate"}, \code{"tritium"},
#' \code{"spice"}, \code{"fluorescence"}, \code{"p"}, \code{"z"},
#' \code{"distance"}, \code{"distance km"}, \code{"along-track distance km"},
#' \code{"heading"}, \code{"pitch"}, \code{"roll"}, \code{"u"}, \code{"v"},
#' \code{"w"}, \code{"speed"}, \code{"direction"}, \code{"eastward"},
#' \code{"northward"}, \code{"depth"}, \code{"elevation"}, \code{"latitude"},
#' \code{"longitude"}, \code{"frequency cph"}, or \code{"spectral density
#' m2/cph"}.
#' @param axis a string indicating which axis to use; must be \code{x} or
#' \code{y}.
#' @param sep optional character string inserted between the unit and the
#' parentheses or brackets that enclose it. If not provided, then
#' \code{\link{getOption}("oceUnitSep")} is checked. If that exists, then it is
#' used as the separator; if not, no separator is used.
#' @param unit optional unit to use, if the default is not satisfactory. This
#' might be the case if for example temperature was not measured in Celcius.
#' @param debug optional debugging flag. Setting to 0 turns off debugging,
#' while setting to 1 causes some debugging information to be printed.
#' @return A character string or expression, in either a long or a shorter
#' format, for use in the indicated axis at the present plot size.  Whether the
#' unit is enclosed in parentheses or square brackets is determined by the
#' value of \code{getOption("oceUnitBracket")}, which may be \code{"["} or
#' \code{"("}.  Whether spaces are used between the unit and these deliminators
#' is set by \code{psep} or \code{\link{getOption}("oceUnitSep")}.
#' @author Dan Kelley
resizableLabel <- function(item, axis="x", sep, unit=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "resizableLabel(item=\"", item,
             "\", axis=\"", axis,
             "\", sep=\"", if (missing(sep)) "(missing)" else sep, "\", ...) {\n",
            sep="", unindent=1)
    if (missing(item))
        stop("must provide 'item'")
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
        if (is.list(unit)) {
            unit <- unit[[1]] # second item is a scale
        }
        if (0 == length(unit) || 0 == nchar(unit))
            unit <- NULL
    }
    ## Previous to 2016-06-11, an error was reported if there was no match.
    itemAllowedMatch <- pmatch(item, itemAllowed)
    if (!is.na(itemAllowedMatch))
        item <- itemAllowed[itemAllowedMatch[1]]
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
    if (item == "T" || item == "temperature") {
        var <- gettext("Temperature", domain="R-oce")
        if (is.null(unit)) {
            ##message("no unit given for temperature")
            full <- bquote(.(var)*.(L)*degree*"C"*.(R))
            abbreviated <- bquote("T"*.(L)*degree*"C"*.(R))
        } else {
            ##message("unit given for temperature")
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote("T"*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "conductivity mS/cm") {
        var <- gettext("Conductivity", domain="R-oce")
        full <- bquote(.(var)*.(L)*mS/cm*.(R))
        abbreviated <- bquote("C"*.(L)*mS/cm*.(R))
    } else if (item == "conductivity S/m") {
        var <- gettext("Conductivity", domain="R-oce")
        full <- bquote(.(var)*.(L)*S/m*.(R))
        abbreviated <- bquote("C"*.(L)*S/m*.(R))
    } else if (item == "C") {
        ## unitless form
        var <- gettext("Conductivity Ratio", domain="R-oce")
        unit <- gettext("unitless", domain="R-oce")
        full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
        abbreviated <- bquote("C")
    } else if (item == "conservative temperature") {
        var <- gettext("Conservative Temperature", domain="R-oce")
        full <- bquote(.(var)*.(L)*degree*"C"*.(R))
        abbreviated <- bquote(Theta*.(L)*degree*"C"*.(R))
    } else if (item == "sigmaTheta") {
        var <- gettext("Potential density anomaly", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[theta]*.(L)*kg/m^3*.(R))
    } else if (item == "sigma0") {
        var <- gettext("Potential density anomaly wrt surface", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[0]*.(L)*kg/m^3*.(R))
    } else if (item == "sigma1") {
        var <- gettext("Potential density anomaly wrt 1000 dbar", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[1]*.(L)*kg/m^3*.(R))
    } else if (item == "sigma2") {
        var <- gettext("Potential density anomaly wrt 2000 dbar", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[2]*.(L)*kg/m^3*.(R))
    } else if (item == "sigma3") {
        var <- gettext("Potential density anomaly wrt 3000 dbar", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[3]*.(L)*kg/m^3*.(R))
    } else if (item == "sigma4") {
        var <- gettext("Potential density anomaly wrt 4000 dbar", domain="R-oce")
        full <- bquote(.(var)*.(L)*kg/m^3*.(R))
        abbreviated <- bquote(sigma[4]*.(L)*kg/m^3*.(R))
    } else if (item == "theta") {
        var <- gettext("Potential Temperature", domain="R-oce")
        full <- bquote(.(var)*.(L)*degree*"C"*.(R))
        abbreviated <- bquote(theta*.(L)*degree*"C"*.(R))
    } else if (item == "tritium") {
        var <- gettext("Tritium", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*Tu*.(R))
            abbreviated <- bquote(phantom()^3*H*.(L)*Tu*.(R))
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(phantom()^3*H*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "nitrate") {
        var <- gettext("Nitrate", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
            abbreviated <- bquote(N*O[3]*.(L)*mu*mol/kg*.(R))
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(N*O[3]*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "nitrite") {
        var <- gettext("Nitrite", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
            abbreviated <- bquote(N*O[2]*.(L)*mu*mol/kg*.(R))
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(N*O[2]*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "oxygen") {
        var <- gettext("oxygen", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var))
            abbreviated <- bquote(O[2])
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(O[2]*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "oxygen saturation") {
        var <- gettext("Oxygen saturation", domain="R-oce")
        full <- bquote(.(var))
        abbreviated <- bquote(O[2]*.(L)*percent*saturation*.(R))
    } else if (item ==  "oxygen mL/L") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mL/L*.(R))
        abbreviated <- bquote(O[2]*.(L)*mL/L*.(R))
    } else if (item == "oxygen umol/L") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/L*.(R))
        abbreviated <- bquote(O[2]*.(L)*mu*mol/L*.(R))
    } else if (item == "oxygen umol/kg") {
        var <- gettext("Oxygen", domain="R-oce")
        full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
        abbreviated <- bquote(O[2]*.(L)*mu*mol/kg*.(R))
    } else if (item == "phosphate") {
        var <- gettext("Phosphate", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
            abbreviated <- bquote(P*O[4]*.(L)*mu*mol/kg*.(R))
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(P*O[4]*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "silicate") {
        var <- gettext("Silicate", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*mu*mol/kg*.(R))
            abbreviated <- bquote(Si*O[4]*.(L)*mu*mol/kg*.(R))
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote(Si*O[4]*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "fluorescence") {
        var <- gettext("Fluorescence", domain="R-oce")
        if (is.null(unit)) {
            ## I've no idea what a 'standard' unit might be
            full <- bquote(.(var))
            abbreviated <- full
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- bquote("Fluor."*.(L)*.(unit[[1]])*.(R))
        }
    } else if (item == "spice") {
        var <- gettext("Spice", domain="R-oce")
        if (is.null(unit)) {
            full <- bquote(.(var)*.(L)*kg/m^3*.(R))
            abbreviated <- full
        } else {
            full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- full
        }
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
        abbreviated <- full <- bquote(.(var)*.(L)*.(unit[[1]])*.(R))
    } else if (item == "spectral density m2/cph") {
        var <- gettext("Spectral density", domain="R-oce")
        full <- bquote(.(var)*.(L)*m^2/cph*.(R))
        var <- gettext("Spec. dens.", domain="R-oce")
        abbreviated <- bquote(.(var)*.(L)*m^2/cph*.(R))
    } else {
        oceDebug(debug, "unknown item=\"", item, "\"\n", sep="")
        if (is.null(unit)) {
            oceDebug(debug, "no unit given\n")
            ##message("no unit given")
            full <- item
            abbreviated <- full
        } else {
            oceDebug(debug, "unit \"", unit, "\" given\n")
            full <- bquote(.(item)*.(L)*.(unit[[1]])*.(R))
            abbreviated <- full
        }
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
    oceDebug(debug, "} # resizableLabel\n", unindent=1)
    if (fraction < 1) full else abbreviated
}


#' Rotate velocity components within an oce object
#'
#' Alter the horizontal components of velocities in \code{adp},
#' \code{adv} or \code{cm} objects, by applying a rotation about
#' the vertical axis.
#'
#' @param x An oce object of class \code{adp}, \code{adv} or \code{cm}.
#' @param angle The rotation angle about the z axis, in degrees counterclockwise.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' par(mfcol=c(2, 3))
#' # adp (acoustic Doppler profiler)
#' data(adp)
#' plot(adp, which="uv")
#' mtext("adp", side=3, line=0, adj=1, cex=0.7)
#' adpRotated <- rotateAboutZ(adp, 30)
#' plot(adpRotated, which="uv")
#' mtext("adp rotated 30 deg", side=3, line=0, adj=1, cex=0.7)
#' # adv (acoustic Doppler velocimeter)
#' data(adv)
#' plot(adv, which="uv")
#' mtext("adv", side=3, line=0, adj=1, cex=0.7)
#' advRotated <- rotateAboutZ(adv, 125)
#' plot(advRotated, which="uv")
#' mtext("adv rotated 125 deg", side=3, line=0, adj=1, cex=0.7)
#' # cm (current meter)
#' data(cm)
#' plot(cm, which="uv")
#' mtext("cm", side=3, line=0, adj=1, cex=0.7)
#' cmRotated <- rotateAboutZ(cm, 30)
#' plot(cmRotated, which="uv")
#' mtext("cm rotated 30 deg", side=3, line=0, adj=1, cex=0.7)
#'
#' @family things related to \code{adp} data
#' @family things related to \code{adv} data
#' @family things related to \code{cm} data
rotateAboutZ <- function(x, angle)
{
    if (missing(angle))
        stop("must supply angle")
    S <- sin(angle * pi / 180)
    C <- cos(angle * pi / 180)
    rotation <- matrix(c(C, S, -S, C), nrow=2)
    res <- x
    allowedClasses <- c("adp", "adv", "cm")
    if (!(class(x) %in% allowedClasses))
        stop("cannot rotate for class \"", class(x), "\"; try one of: \"",
             paste(allowedClasses, collapse="\" \""), "\")")
    if (inherits(x, "adp")) {
        if (x[["oceCoordinate"]] != "enu")
            stop("cannot rotate adp unless coordinate system is 'enu'; see ?toEnu or ?xyzToEnu")
        V <- x[["v"]]
        ## Work through the bins, transforming a 3D array operation to a
        ## sequence of 2D matrix operations.
        for (j in seq_len(dim(V)[2])) {
            uvr <- rotation %*% t(V[, j, 1:2])
            V[, j, 1] <- uvr[1, ]
            V[, j, 2] <- uvr[2, ]
        }
        res@data$v <- V
    } else if (inherits(x, "adv")) {
        if (x[["oceCoordinate"]] != "enu")
            stop("cannot rotate adv unless coordinate system is 'enu'; see ?toEnu or ?xyzToEnu")
        V <- x[["v"]]
        uvr <- rotation %*% t(V[, 1:2])
        V[, 1] <- uvr[1, ]
        V[, 2] <- uvr[2, ]
        res@data$v <- V
    } else if (inherits(x, "cm")) {
        uvr <- rotation %*% rbind(x@data$u, x@data$v)
        res@data$u <- uvr[1, ]
        res@data$v <- uvr[2, ]
    } else {
        stop("cannot rotate for class \"", class(x), "\"; try one of: \"",
             paste(allowedClasses, collapse="\" \""), "\". (internal error: please report)")
    }
    ## Update processing log
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("rotateAboutZ(x, angle=", angle, ")", sep=""))
    res
}

#' Format a latitude-longitude pair
#'
#' Format a latitude-longitude pair, using "S" for negative latitudes, etc.
#'
#' @param lat latitude in \eqn{^\circ}{deg}N north of the equator.
#' @param lon longitude in \eqn{^\circ}{deg}N east of Greenwich.
#' @param digits the number of significant digits to use when printing.
#' @return A character string.
#' @author Dan Kelley
#' @seealso \code{\link{latFormat}} and \code{\link{lonFormat}}.
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


#' Format a latitude
#'
#' Format a latitude, using "S" for negative latitude.
#'
#' @param lat latitude in \eqn{^\circ}{deg}N north of the equator.
#' @param digits the number of significant digits to use when printing.
#' @return A character string.
#' @author Dan Kelley
#' @seealso \code{\link{lonFormat}} and \code{\link{latlonFormat}}.
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


#' Format a longitude
#'
#' Format a longitude, using "W" for west longitude.
#'
#' @param lon longitude in \eqn{^\circ}{deg}N east of Greenwich.
#' @param digits the number of significant digits to use when printing.
#' @return A character string.
#' @author Dan Kelley
#' @seealso \code{\link{latFormat}} and \code{\link{latlonFormat}}.
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


#' Determine time offset from timezone
#'
#' The data are from
#' \url{http://www.timeanddate.com/library/abbreviations/timezones/} and were
#' hand-edited to develop this code, so there may be errors.  Also, note that
#' some of these contradict; if you examine the code, you'll see some
#' commented-out portions that represent solving conflicting definitions by
#' choosing the more common timezone abbreviation over a the less common one.
#'
#' @param tz a timezone, e.g. \code{UTC}.
#' @return Number of hours in offset, e.g. \code{AST} yields 4.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' cat("Atlantic Standard Time is ", GMTOffsetFromTz("AST"), "hours after UTC")
#' @family functions relating to time
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


#' Acceleration due to earth gravity
#'
#' Compute \eqn{g}{g}, the acceleration due to gravity, as a function of
#' latitude.
#'
#' Value not verified yet, except roughly.
#'
#' @param latitude Latitude in \eqn{^\circ}{deg}N or radians north of the
#' equator.
#' @param degrees Flag indicating whether degrees are used for latitude; if set
#' to \code{FALSE}, radians are used.
#' @return Acceleration due to gravity [\eqn{m^2/s}{m^2/s}].
#' @author Dan Kelley
#' @references Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic
#' Press, New York, 662 pp.
#'
#' \strong{Caution:} Fofonoff and Millard (1983 UNESCO) use a different
#' formula.
#' @examples
#' g <- gravity(45) # 9.8
gravity <- function(latitude=45, degrees=TRUE)
{
    if (degrees) latitude <- latitude * 0.0174532925199433
    9.780318 * ( 1.0 + 5.3024e-3 * sin(latitude)^2 - 5.9e-6 * sin(2*latitude)^2 )
}


#' Make a digital filter
#'
#' The filter is suitable for use by \code{\link{filter}},
#' \code{\link{convolve}} or (for the \code{asKernal=TRUE} case) with
#' \code{\link{kernapply}}.  Note that \code{\link{convolve}} should be faster
#' than \code{\link{filter}}, but it cannot be used if the time series has
#' missing values.  For the Blackman-Harris filter, the half-power frequency is
#' at \code{1/m} cycles per time unit, as shown in the \dQuote{Examples}
#' section.  When using \code{\link{filter}} or \code{\link{kernapply}} with
#' these filters, use \code{circular=TRUE}.
#'
#' @param type a string indicating the type of filter to use.  (See Harris
#' (1978) for a comparison of these and similar filters.)  \itemize{ \item
#' \code{"blackman-harris"} yields a modified raised-cosine filter designated
#' as "4-Term (-92 dB) Blackman-Harris" by Harris (1978; coefficients given in
#' the table on page 65).  This is also called "minimum 4-sample Blackman
#' Harris" by that author, in his Table 1, which lists figures of merit as
#' follows: highest side lobe level -92dB; side lobe fall off -6 db/octave;
#' coherent gain 0.36; equivalent noise bandwidth 2.00 bins; 3.0-dB bandwidth
#' 1.90 bins; scallop loss 0.83 dB; worst case process loss 3.85 dB; 6.0-db
#' bandwidth 2.72 bins; overlap correlation 46 percent for 75\% overlap and 3.8
#' for 50\% overlap.  Note that the equivalent noise bandwidth is the width of
#' a spectral peak, so that a value of 2 indicates a cutoff frequency of
#' \code{1/m}, where \code{m} is as given below.  \item \code{"rectangular"}
#' for a flat filter.  (This is just for convenience.  Note that
#' \code{\link{kernel}("daniell",....)} gives the same result, in kernel form.)
#' \code{"hamming"} for a Hamming filter (a raised-cosine that does not taper
#' to zero at the ends) \item \code{"hann"} (a raised cosine that tapers to
#' zero at the ends).  }
#' @param m length of filter.  This should be an odd number, for any
#' non-rectangular filter.
#' @param asKernel boolean, set to \code{TRUE} to get a smoothing kernel for
#' the return value.
#' @return If \code{asKernel} is \code{FALSE}, this returns a list of filter
#' coefficients, symmetric about the midpoint and summing to 1.  These may be
#' used with \code{\link{filter}}, which should be provided with argument
#' \code{circular=TRUE} to avoid phase offsets.  If \code{asKernel} is
#' \code{TRUE}, the return value is a smoothing kernel, which can be applied to
#' a timeseries with \code{\link{kernapply}}, whose bandwidth can be determined
#' with \code{\link{bandwidth.kernel}}, and which has both print and plot
#' methods.
#' @author Dan Kelley
#' @references F. J. Harris, 1978.  On the use of windows for harmonic analysis
#' with the discrete Fourier Transform.  \emph{Proceedings of the IEEE}, 66(1),
#' 51-83 (\url{http://web.mit.edu/xiphmont/Public/windows.pdf}.)
#' @examples
#'
#' library(oce)
#'
#' # 1. Demonstrate step-function response
#' y <- c(rep(1, 10), rep(-1, 10))
#' x <- seq_along(y)
#' plot(x, y, type='o', ylim=c(-1.05, 1.05))
#' BH <- makeFilter("blackman-harris", 11, asKernel=FALSE)
#' H <- makeFilter("hamming", 11, asKernel=FALSE)
#' yBH <- stats::filter(y, BH)
#' points(x, yBH, col=2, type='o')
#' yH <- stats::filter(y, H)
#' points(yH, col=3, type='o')
#' legend("topright", col=1:3, cex=2/3, pch=1,
#'        legend=c("input", "Blackman Harris", "Hamming"))
#'
#' # 2. Show theoretical and practical filter gain, where
#' #    the latter is based on random white noise, and
#' #    includes a particular value for the spans
#' #    argument of spectrum(), etc.
#' \dontrun{ # need signal package for this example
#' r <- rnorm(2048)
#' rh <- stats::filter(r, H)
#' rh <- rh[is.finite(rh)] # kludge to remove NA at start/end
#' sR <- spectrum(r, plot=FALSE, spans=c(11, 5, 3))
#' sRH <- spectrum(rh, plot=FALSE, spans=c(11, 5, 3))
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(sR$freq, sRH$spec/sR$spec, xlab="Frequency", ylab="Power Transfer",
#'      type='l', lwd=5, col='gray')
#' theory <- freqz(H, n=seq(0,pi,length.out=100))
#' # Note we must square the modulus for the power spectrum
#' lines(theory$f/pi/2, Mod(theory$h)^2, lwd=1, col='red')
#' grid()
#' legend("topright", col=c("gray", "red"), lwd=c(5, 1), cex=2/3,
#'        legend=c("Practical", "Theory"), bg="white")
#' plot(log10(sR$freq), log10(sRH$spec/sR$spec),
#'      xlab="log10 Frequency", ylab="log10 Power Transfer",
#'      type='l', lwd=5, col='gray')
#' theory <- freqz(H, n=seq(0,pi,length.out=100))
#' # Note we must square the modulus for the power spectrum
#' lines(log10(theory$f/pi/2), log10(Mod(theory$h)^2), lwd=1, col='red')
#' grid()
#' legend("topright", col=c("gray", "red"), lwd=c(5, 1), cex=2/3,
#'        legend=c("Practical", "Theory"), bg="white")
#


#' }
makeFilter <- function(type=c("blackman-harris", "rectangular", "hamming", "hann"), m, asKernel=TRUE)
{
    type <- match.arg(type)
    if (missing(m))
        stop("must supply 'm'")
    i <- seq(0, m - 1)
    if (type == "blackman-harris") {
        ## See Harris (1978) table on p65
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


#' Filter a time-series
#'
#' Filter a time-series, possibly recursively
#'
#' The filter is defined as e.g.  \eqn{y[i]=b[1]*x[i] + }{y[i]=b[1]*x[i] +
#' b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] - a[3]*y[i-2] - a[4]*y[i-3] -
#' ...}\eqn{ b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] - a[3]*y[i-2] -
#' }{y[i]=b[1]*x[i] + b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] -
#' a[3]*y[i-2] - a[4]*y[i-3] - ...}\eqn{ a[4]*y[i-3] - ...}{y[i]=b[1]*x[i] +
#' b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] - a[3]*y[i-2] - a[4]*y[i-3] -
#' ...}, where some of the illustrated terms will be omitted if the lengths of
#' \code{a} and \code{b} are too small, and terms are dropped at the start of
#' the time series where the index on \code{x} would be less than 1.
#'
#' By contrast with the \code{\link{filter}} function of R, \code{oce.filter}
#' lacks the option to do a circular filter.  As a consequence,
#' \code{oceFilter} introduces a phase lag.  One way to remove this lag is to
#' run the filter forwards and then backwards, as in the \dQuote{Examples}.
#' However, the result is still problematic, in the sense that applying it in
#' the reverse order would yield a different result.  (Matlab's \code{filtfilt}
#' shares this problem.)
#'
#' @aliases oce.filter
#' @param x a vector of numeric values, to be filtered as a time series.
#' @param a a vector of numeric values, giving the \eqn{a}{a} coefficients (see
#' \dQuote{Details}).
#' @param b a vector of numeric values, giving the \eqn{b}{b} coefficients (see
#' \dQuote{Details}).
#' @param zero.phase boolean, set to \code{TRUE} to run the filter forwards,
#' and then backwards, thus removing any phase shifts associated with the
#' filter.
#' @return A numeric vector of the filtered results, \eqn{y}{y}, as denoted in
#' \dQuote{Details}.
#' @note The first value in the \code{a} vector is ignored, and if
#' \code{length(a)} equals 1, a non-recursive filter results.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' par(mar=c(4, 4, 1, 1))
#' b <- rep(1, 5)/5
#' a <- 1
#' x <- seq(0, 10)
#' y <- ifelse(x == 5, 1, 0)
#' f1 <- oceFilter(y, a, b)
#' plot(x, y, ylim=c(-0, 1.5), pch="o", type='b')
#' points(x, f1, pch="x", col="red")
#'
#' # remove the phase lag
#' f2 <- oceFilter(y, a, b, TRUE)
#' points(x, f2, pch="+", col="blue")
#'
#' legend("topleft", col=c("black","red","blue"), pch=c("o","x","+"),
#'        legend=c("data","normal filter", "zero-phase filter"))
#' mtext("note that normal filter rolls off at end")
#'
#'
oceFilter <- function(x, a=1, b, zero.phase=FALSE)
{
    if (missing(x))
        stop("must supply x")
    if (missing(b))
        stop("must supply b")
    if (!zero.phase) {
        return(do_oce_filter(x, a, b))
    } else {
        res <- do_oce_filter(x, a, b)
        res <- rev(res)
        res <- do_oce_filter(res, a, b)
        return(rev(res))
    }
}
oce.filter <- oceFilter


#' Grid data using Barnes algorithm
#'
#' The algorithm follows that described by Koch et al. (1983), with the
#' addition of the ability to blank out the grid in spots where data are
#' sparse, using the \code{trim} argument, and the ability to pre-grid, with
#' the \code{pregrid} argument.
#'
#' @param x,y a vector of x and ylocations.
#' @param z a vector of z values, one at each (x,y) location.
#' @param w a optional vector of weights at the (x,y) location.  If not
#' supplied, then a weight of 1 is used for each point, which means equal
#' weighting.  Higher weights give data points more influence.
#' @param xg,yg optional vectors defining the x and y grids.  If not supplied,
#' these values are inferred from the data, using e.g. \code{pretty(x, n=50)}.
#' @param xgl,ygl optional lengths of the x and y grids, to be constructed with
#' \code{\link{seq}} spanning the data range.  These values \code{xgl} are only
#' examined if \code{xg} and \code{yg} are not supplied.
#' @param xr,yr optional values defining the width of the radius ellipse in the
#' x and y directions.  If not supplied, these are calculated as the span of x
#' and y over the square root of the number of data.
#' @param gamma grid-focussing parameter.  At each iteration, \code{xr} and
#' \code{yr} are reduced by a factor of \code{sqrt(gamma)}.
#' @param iterations number of iterations.
#' @param trim a number between 0 and 1, indicating the quantile of data weight
#' to be used as a criterion for blanking out the gridded value (using
#' \code{NA}).  If 0, the whole \code{zg} grid is returned.  If >0, any spots
#' on the grid where the data weight is less than the \code{trim}-th
#' \code{\link{quantile}} are set to \code{NA}.  See examples.
#' @param pregrid an indication of whether to pre-grid the data. If
#' \code{FALSE}, this is not done, i.e. conventional Barnes interpolation is
#' performed.  Otherwise, then the data are first averaged within grid cells
#' using \code{\link{binMean2D}}.  If \code{pregrid} is \code{TRUE} or
#' \code{4}, then this averaging is done within a grid that is 4 times finer
#' than the grid that will be used for the Barnes interpolation. Otherwise,
#' \code{pregrid} may be a single integer indicating the grid refinement (4
#' being the result if \code{TRUE} had been supplied), or a vector of two
#' integers, for the grid refinement in x and y. The purpose of using
#' \code{pregrid} is to speed processing on large datasets, and to remove
#' spatial bias (e.g. with a single station that is repeated frequently in an
#' otherwise seldom-sampled region).  A form of pregridding is done in the
#' World Ocean Atlas, for example.
#' @param debug a flag that turns on debugging.  Set to 0 for no debugging
#' information, to 1 for more, etc; the value is reduced by 1 for each
#' descendent function call.
#' @return A list containing: \code{xg}, a vector holding the x-grid);
#' \code{yg}, a vector holding the y-grid; \code{zg}, a matrix holding the
#' gridded values; \code{wg}, a matrix holding the weights used in the
#' interpolation at its final iteration; and \code{zd}, a vector of the same
#' length as \code{x}, which holds the interpolated values at the data points.
#' @author Dan Kelley
#' @seealso See \code{\link{wind}}.
#' @references S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
#' interactive Barnes objective map analysis scheme for use with satellite and
#' conventional data,'' \emph{J.  Climate Appl.  Met.}, vol 22, p. 1487-1503.
#' @examples
#'
#' library(oce)
#'
#' # 1. contouring example, with wind-speed data from Koch et al. (1983)
#' data(wind)
#' u <- interpBarnes(wind$x, wind$y, wind$z)
#' contour(u$xg, u$yg, u$zg, labcex=1)
#' text(wind$x, wind$y, wind$z, cex=0.7, col="blue")
#' title("Numbers are the data")
#'
#' # 2. As 1, but blank out spots where data are sparse
#' u <- interpBarnes(wind$x, wind$y, wind$z, trim=0.1)
#' contour(u$xg, u$yg, u$zg, level=seq(0, 30, 1))
#' points(wind$x, wind$y, cex=1.5, pch=20, col="blue")
#'
#' # 3. As 1, but interpolate back to points, and display the percent mismatch
#' u <- interpBarnes(wind$x, wind$y, wind$z)
#' contour(u$xg, u$yg, u$zg, labcex=1)
#' mismatch <- 100 * (wind$z - u$zd) / wind$z
#' text(wind$x, wind$y, round(mismatch), col="blue")
#' title("Numbers are percent mismatch between grid and data")
#'
#'
#' # 4. As 3, but contour the mismatch
#' mismatchGrid <- interpBarnes(wind$x, wind$y, mismatch)
#' contour(mismatchGrid$xg, mismatchGrid$yg, mismatchGrid$zg, labcex=1)
#'
#' # 5. One-dimensional example, smoothing a salinity profile
#' data(ctd)
#' p <- ctd[["pressure"]]
#' y <- rep(1, length(p)) # fake y data, with arbitrary value
#' S <- ctd[["salinity"]]
#' pg <- pretty(p, n=100)
#' g <- interpBarnes(p, y, S, xg=pg, xr=1)
#' plot(S, p, cex=0.5, col="blue", ylim=rev(range(p)))
#' lines(g$zg, g$xg, col="red")
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
                            xbreaks=seq(xg[1], tail(xg, 1), (xg[2]-xg[1]) / pregrid[1]),
                            ybreaks=seq(yg[1], tail(yg, 1), (yg[2]-yg[1]) / pregrid[2]),
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
                        xbreaks=seq(xg[1], tail(xg, 1), (xg[2]-xg[1])/pregrid[1]),
                        ybreaks=seq(yg[1], tail(yg, 1), (yg[2]-yg[1])/pregrid[2]),
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
    g <- do_interp_barnes(x[ok], y[ok], z[ok], w[ok], xg, yg, xr, yr, gamma, iterations)
    oceDebug(debug, "} # interpBarnes(...)\n", unindent=1)
    if (trim >= 0 && trim <= 1) {
        bad <- g$wg < quantile(g$wg, trim, na.rm=TRUE)
        g$zg[bad] <- NA
    }
    list(xg=xg, yg=yg, zg=g$zg, wg=g$wg, zd=g$zd)
}

#' Coriolis parameter on rotating earth
#'
#' Compute \eqn{f}{f}, the Coriolis parameter as a function of latitude [1],
#' assuming earth siderial angular rotation rate
#' \eqn{omega}{omega}=7292115e-11 rad/s. See [1] for general notes, and
#' see [2] for comments on temporal variations
#' of \eqn{omega}{omega}.
#'
#' @param latitude Vector of latitudes in \eqn{^\circ}{deg}N or radians north of the equator.
#' @param degrees Flag indicating whether degrees are used for latitude; if set
#' to \code{FALSE}, radians are used.
#' @return Coriolis parameter [radian/s].
#' @author Dan Kelley
#' @references
#' 1. Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic
#' Press, New York, 662 pp.
#'
#' 2. Groten, E., 2004: Fundamental Parameters and Current, 2004. Best
#'  Estimates of the Parameters of Common Relevance to Astronomy, Geodesy,
#'  and Geodynamics. Journal of Geodesy, 77:724-797.
#'  (downloaded from
#' \code{http://www.iag-aig.org/attach/e354a3264d1e420ea0a9920fe762f2a0/51-groten.pdf}
#' March 11, 2017).
#' @examples
#' C <- coriolis(45) # 1e-4
coriolis <- function(latitude, degrees=TRUE)
{
    ## Siderial day 86164.1 s.
    if (degrees) latitude <- latitude * 0.0174532925199433
    ## http://www.iag-aig.org/attach/e354a3264d1e420ea0a9920fe762f2a0/51-groten.pdf 7292115e-11
    2 * 7292115e-11 * sin(latitude)
}



#' Correct for drift in instrument clock
#'
#' It is assumed that the instrument clock matches the real time at the start
#' of the sampling, and that the clock drifts linearly (i.e. is uniformly fast
#' or slow) over the sampling interval.  Linear interpolation is used to infer
#' the values of all variables in the \code{data} slot.  The data length is
#' altered in this process, e.g. a slow instrument clock (positive
#' \code{slowEnd}) takes too few samples in a given time interval, so
#' \code{undriftTime} will increase the number of data.
#'
#' @param x an object of \code{\link{oce-class}}.
#' @param slowEnd number of seconds to add to final instrument time, to get the
#' correct time of the final sample.  This will be a positive number, for a
#' "slow" instrument clock.
#' @param tname Character string indicating the name of the time column in the
#' \code{data} slot of \code{x}.
#' @return An object of the same class as \code{x}, with the \code{data} slot
#' adjusted appropriately.
#' @author Dan Kelley
#' @examples
#' \dontrun{
#' library(oce)
#' rbr011855 <- read.oce(
#'  "/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/raw/pt_rbr_011855.dat")
#' d <- subset(rbr011855, time < as.POSIXct("2008-06-25 10:05:00"))
#' x <- undriftTime(d, 1)   # clock lost 1 second over whole experiment
#' summary(d)
#' summary(x)
#' }
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




#' Fill a gap in an oce object
#'
#' Sequences of \code{NA} values, are filled by linear interpolation between
#' the non-\code{NA} values that bound the gap.
#'
#' @param x an \code{oce} object.
#' @param method to use; see \dQuote{Details}.
#' @param rule integer controlling behaviour at start and end of \code{x}.  If
#' \code{rule=1}, \code{NA} values at the ends are left in the return value.
#' If \code{rule=2}, they are replaced with the nearest non-NA point.
#' @return A new \code{oce} object, with gaps removed.
#'
#' @section Bugs:
#' \enumerate{
#' \item Eventually, this will be expanded to work
#' with any \code{oce} object.  But, for now, it only works for vectors that
#' can be coerced to numeric.
#' \item If the first or last point is \code{NA}, then \code{x} is returned unaltered.
#' \item Only method \code{linear} is permitted now.
#' }
#' @author Dan Kelley
#' @examples
#' library(oce)
#' # Integers
#' x <- c(1:2, NA, NA, 5:6)
#' y <- fillGap(x)
#' print(data.frame(x,y))
#' # Floats
#' x <- x + 0.1
#' y <- fillGap(x)
#' print(data.frame(x,y))
fillGap <- function(x, method=c("linear"), rule=1)
{
    if (!is.numeric(x))
        stop("only works for numeric 'x'")
    method <- match.arg(method)
    class <- class(x)
    if (is.vector(x)) {
        ##res <- .Call("fillgap1d", as.numeric(x), rule)
        res <- do_fill_gap_1d(x, rule)
    } else if (is.matrix(x))  {
        res <- x
        for (col in seq_len(ncol(x)))
            res[, col] <- do_fill_gap_1d(x[, col], rule)
        for (row in seq_len(nrow(x)))
            res[row, ] <- do_fill_gap_1d(x[row, ], rule)
    } else {
        stop("only works if 'x' is a vector or a matrix")
    }
    class(res) <-  class
    res
}


#' Smooth and Decimate, or Subsample, an Oce Object
#'
#' Later on, other methods will be added, and \code{\link{ctdDecimate}} will be
#' retired in favour of this, a more general, function.  The filtering is done
#' with the \code{\link{filter}} function of the stats package.
#'
#' @param x an \code{oce} object containing a \code{data} element.
#' @param by an indication of the subsampling.  If this is a single number,
#' then it indicates the spacing between elements of \code{x} that are
#' selected.  If it is two numbers (a condition only applicable if \code{x} is
#' an \code{echosounder} object, at present), then the first number indicates
#' the time spacing and the second indicates the depth spacing.
#' @param to Indices at which to subsample.  If given, this over-rides
#' \code{by}.
#' @param filter optional list of numbers representing a digital filter to be
#' applied to each variable in the \code{data} slot of \code{x}, before
#' decimation is done. If not supplied, then the decimation is done strictly by
#' sub-sampling.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @return An object of \code{\link[base]{class}} \code{"oce"} that has been
#' subsampled appropriately.
#' @section Bugs: Only a preliminary version of this function is provided in
#' the present package.  It only works for objects of class \code{echosounder},
#' for which the decimation is done after applying a running median filter and
#' then a boxcar filter, each of length equal to the corresponding component of
#' \code{by}.
#' @author Dan Kelley
#' @seealso Filter coefficients may be calculated using
#' \code{\link{makeFilter}}.  (Note that \code{\link{ctdDecimate}} will be
#' retired when the present function gains equivalent functionality.)
#' @examples
#' library(oce)
#' data(adp)
#' plot(adp)
#' adpDec <- decimate(adp,by=2,filter=c(1/4, 1/2, 1/4))
#' plot(adpDec)
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
        if (length(by) == 1) {
            ## FIXME: probably should not be here
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
                        res@data[[name]][, j] <- filterSomething(x@data[[name]][, j], filter)
                    res@data[[name]][, j] <- res@data[[name]][, j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                print(dim)
                for (k in 1:dim[2]) {
                    for (j in 1:dim[3]) {
                        oceDebug(debug, "subsetting x@data[[", name, "]][", j, ",", k, "], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][, j, k] <- filterSomething(x@data[[name]][, j, k], filter)
                        res@data[[name]][, j, k] <- res@data[[name]][, j, k][select]
                    }
                }
            }
        }
    } else if (inherits(x, "adv")) {
        ## FIXME: the (newer) adp code is probably better than this ADV code
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
                        res@data[[name]][, j] <- filterSomething(x@data[[name]][, j], filter)
                    res@data[[name]][, j] <- res@data[[name]][, j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "decimating x@data[[", name, ",", j, ",", k, "]], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][, j, k] <- filterSomething(x@data[[name]][, j, k], filter)
                        res@data[[name]][, j, k] <- res@data[[name]][, j, k][select]
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
        res@data <- x@data[select, ]
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
                a2[r, ] <- binAverage(ii, runmed(a[r, ], kDepth), 1, ncol, byDepth)$y
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
                a2[, c] <- binAverage(jj, runmed(a[, c], kPing), 1, nrow, byPing)$y
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



#' Smooth an Oce Object
#'
#' Each data element is smoothed as a timeseries. For ADP data, this is done
#' along time, not distance.  Time vectors, if any, are not smoothed.  A good
#' use of \code{oce.smooth} is for despiking noisy data.
#'
#' @aliases oce.smooth
#' @param x an \code{oce} object.
#' @param \dots parameters to be supplied to \code{\link{smooth}}, which does
#' the actual work.
#' @return An object of \code{\link[base]{class}} \code{"oce"} that has been
#' smoothed appropriately.
#' @author Dan Kelley
#' @seealso The work is done with \code{\link{smooth}}, and the \code{...}
#' arguments are handed to it directly by \code{oce.smooth}.
#' @examples
#' library(oce)
#' data(ctd)
#' d <- oce.smooth(ctd)
#' plot(d)
oceSmooth <- function(x, ...)
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
                    res@data[[name, j]] <- smooth(x@data[[name, j]], ...)
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "smoothing x@data[[", name, ",", j, "]], which is an arry \n", sep="")
                        res@data[[name, j, k]] <- smooth(x@data[[name, j, k]], ...)
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
oce.smooth <- oceSmooth



#' Decode BCD to integer
#'
#' @param x a raw value, or vector of raw values, coded in binary-coded
#' decimal.
#' @param endian character string indicating the endian-ness ("big" or
#' "little").  The PC/intel convention is to use "little", and so most data
#' files are in that format.
#' @return An integer, or list of integers.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' twenty.five <- bcdToInteger(as.raw(0x25))
#' thirty.seven <- as.integer(as.raw(0x25))
bcdToInteger <- function(x, endian=c("little", "big"))
{
    endian <- match.arg(endian)
    x <- as.integer(x)
    byte1 <- as.integer(floor(x / 16))
    byte2 <- x - 16 * byte1
    if (endian=="little") 10*byte1 + byte2 else byte1 + 10*byte2
}


#' Format bytes as binary [defunct]
#'
#' \strong{WARNING:} The \code{endian} argument will soon be removed
#' from this function; see \link{oce-defunct}.
#' This is because the actions for \code{endian="little"} made
#' no sense in practical work. The default value for \code{endian}
#' was changed to \code{"big"} on 2017 May 6.
#'
#' @param x an integer to be interpreted as a byte.
#' @param endian character string indicating the endian-ness ("big" or
#' "little"). \strong{This argument will be removed in the upcoming CRAN
#' release.}
#' @return A character string representing the bit strings for the elements of
#' \code{x}, in order of significance for the \code{endian="big"} case.
#' (The nibbles, or 4-bit sequences, are interchanged in the now-deprecated
#' \code{"little"} case.)
#' See \dQuote{Examples} for how this relates to the output from
#' \link{rawToBits}.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' ## Note comparison with rawToBits():
#' a <- as.raw(0x0a)
#' byteToBinary(a, "big")        # "00001010"
#' as.integer(rev(rawToBits(a))) # 0 0 0 0 1 0 1 0
byteToBinary <- function(x, endian="big")
{
    if (endian != "big")
        .Defunct("rawToBits",
                 msg="byteToBinary(.,'little') is disallowed and will be removed soon. See ?'oce-defunct'.")
    ## onebyte2binary <- function(x)
    ## {
    ##     c("0000", "0001", "0010", "0011",
    ##       "0100", "0101", "0110", "0111",
    ##       "1000", "1001", "1010", "1011",
    ##       "1100", "1101", "1110", "1111")[x+1]
    ## }
    ## res <- NULL
    ## if (class(x) == "raw")
    ##     x <- readBin(x, "int", n=length(x), size=1, signed=FALSE)
    ## for (i in seq_along(x)) {
    ##     if (x[i] < 0) {
    ##         res <- c(res, "??")
    ##     } else {
    ##         ## FIXME: these are not bytes here; they are nibbles.  I don't think endian="little"
    ##         ## makes ANY SENSE at all.  2012-11-22
    ##         byte1 <- as.integer(floor(x[i] / 16))
    ##         byte2 <- x[i] - 16 * byte1
    ##         ##cat("input=",x[i],"byte1=",byte1,"byte2=",byte2,"\n")
    ##         if (endian == "little")
    ##             res <- c(res, paste(onebyte2binary(byte2), onebyte2binary(byte1), sep=""))
    ##         else
    ##             res <- c(res, paste(onebyte2binary(byte1), onebyte2binary(byte2), sep=""))
    ##         ##cat(" res=",res,"\n")
    ##     }
    ## }
    ## res
    x <- as.raw(x)
    paste(ifelse(rev(rawToBits(x)==as.raw(0x01)), "1", "0"),collapse="")
}


#' Confidence interval in parenthetic notation
#'
#' Format a confidence interval in parenthetic notation.
#'
#' If a \code{model} is given, then \code{ci} is ignored, and a confidence
#' interval is calculated using \code{\link{confint}} with \code{level} set to
#' 0.6914619.  This \code{level} corresponds to a range of plus or minus one
#' standard deviation, for the t distribution and a large number of degrees of
#' freedom (since \code{qt(0.6914619, 100000)} is 0.5).
#'
#' If \code{model} is missing, \code{ci} must be provided.  If it contains 3
#' elements, then first and third elements are taken as the range of the
#' confidence interval (which by convention should use the \code{level} stated
#' in the previous paragraph), and the second element is taken as the central
#' value.  Alternatively, if \code{ci} has 2 elements, they are taken to be
#' bounds of the confidence interval and their mean is taken to be the central
#' value.
#'
#' In the \code{+/-} notation, e.g. \eqn{a \pm b}{a +/- b} means that the true
#' value lies between \eqn{a-b}{a-b} and \eqn{a+b}{a+b} with a high degree of
#' certainty.  Mills et al. (1993, section 4.1 on page 83) suggest that
#' \eqn{b}{b} should be set equal to 2 times the standard uncertainty or
#' standard deviation.  JCGM (2008, section 7.2.2 on pages 25 and 26), however,
#' suggest that \eqn{b}{b} should be set to the standard uncertainty, while
#' also recommending that the \eqn{\pm}{+/-} notation be avoided altogether.
#'
#' The \code{parentheses} notation is often called the compact notation.  In
#' it, the digits in parentheses indicate the uncertainty in the corresponding
#' digits to their left, e.g. 12.34(3) means that the last digit (4) has an
#' uncertainty of 3.  However, as with the \eqn{\pm}{+/-} notation, different
#' authorities offer different advice on defining this uncertainty; Mills et
#' al. (1993, section 4.1 on page 83) provide an example in which the
#' parenthetic notation has the same value as the \eqn{\pm}{+/-} notation,
#' while JCM (2008, section 7.2.2 on pages 25 and 26) suggest halving the
#' number put in parentheses.
#'
#' The \code{foramtci} function is based on the JCM (2008) notation, i.e.
#' \code{formatCI(ci=c(8,12), style="+/-")} yields \code{"10+-2"}, and
#' \code{formatCI(ci=c(8,12), style="parentheses")} yields \code{"10(2)"}.
#'
#' \strong{Note:} if the confidence range exceeds the value, the
#' \code{parentheses} format reverts to \code{+/-} format.
#'
#' @param ci optional vector of length 2 or 3.
#' @param style string indicating notation to be used.
#' @param model optional regression model, e.g. returned by \code{\link{lm}} or
#' \code{\link{nls}}.
#' @param digits optional number of digits to use; if not supplied,
#' \code{\link{getOption}("digits")} is used.
#' @return If \code{ci} is given, the result is a character string with the
#' estimate and its uncertainty, in plus/minus or parenthetic notation.  If
#' \code{model} is given, the result is a 1-column matrix holding character
#' strings, with row names corresponding to the parameters of the model.
#' @author Dan Kelley
#' @references JCGM, 2008.  \emph{Evaluation of measurement data - Guide to the
#' expression of uncertainty in measurement (JCGM 100:2008)}, published by the
#' Joint Committee for Guides in Metrology.
#' [\url{http://www.bipm.org/en/publications/guides/gum.html}] (See section
#' 7.2.2 for a summary of notation, which shows equal values to the right of a
#' \code{+-} sign and in parentheses.)
#'
#' I. Mills, T. Cvitas, K. Homann, N. Kallay, and K. Kuchitsu, 1993.
#' \emph{Quantities, Units and Symbols in Physical Chemistry}, published
#' Blackwell Science for the International Union of Pure and Applied Chemistry.
#' (See section 4.1, page 83, for a summary of notation, which shows that a
#' value to the right of a \code{+-} sign is to be halved if put in
#'
#' @examples
#' x <- seq(0, 1, length.out=300)
#' y <- rnorm(n=300, mean=10, sd=1) * x
#' m <- lm(y~x)
#' print(formatCI(model=m))
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
            ci <- ci[c(1, 3)]
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
            ##x0 <- x / scale
            ##ci0 <- ci / scale
            if (pm > x) return(paste(sign*x, "+/-", pm, sep=""))
            digits <- floor(log10(scale) + 0.1)
            if (digits < 0)
                fmt <- paste("%.", abs(digits), "f", sep="")
            else
                fmt <- "%.f"
            oceDebug(debug, "pm=", pm, ";pmr=", pmr, "; scale=", scale, "pm/scale=", pm/scale, "round(pm/scale)=", round(pm/scale), "\n", " x=", x,  "; x/scale=", x/scale, "digits=", digits, "fmt=", fmt, "\n")
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
                res[row, 1] <- formatCI.one(ci=ci[row, ], style=style, digits=digits)
            }
        }
        res
    } else {
        if (missing(ci))
            stop("must give either ci or model")
        formatCI.one(ci=ci, style=style, digits=digits)
    }
}


#' Decode integer to corresponding ASCII code
#'
#' @param i an integer, or integer vector.
#' @return A character, or character vector.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' A <- integerToAscii(65)
#' cat("A=", A, "\n")
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


#' Earth magnetic declination, inclination, and intensity
#'
#' Implements the 12th generation International Geomagnetic Reference Field
#' (IGRF), based on a reworked version of a Fortran program downloaded from a
#' NOAA website [1].
#'
#' The code (subroutine \code{igrf12syn}) seems to have
#' been written by Susan Macmillan of the British Geological Survey.  Comments
#' in the source code indicate that it employs coefficients agreed to in
#' December 2014 by the IAGA Working Group V-MOD.  Other comments in that code
#' suggest that the valid time interval is from years 1900 to 2020,
#' with only the values from 1945 to 2010 being considered definitive.
#'
#' Reference [2] suggest that a new version to the underlying source
#' code might be expected in 2019 or 2020, but a check on January 31,
#' 2019, showed that version 12, as incoporated in oce since 
#' 2015, remains the active version.
#'
#' @param longitude longitude in degrees east (negative for degrees west).  The
#' dimensions must conform to lat.
#' @param latitude latitude in degrees north, a number, vector, or matrix.
#' @param time either a decimal year or a POSIX time corresponding to the
#' \code{longitude} and \code{latitude} values, or a vector or matrix matching
#' these location values.
#' @return A list containing \code{declination}, \code{inclination}, and
#' \code{intensity}.
#' @author Dan Kelley wrote the R code and a fortran wrapper to the
#' \code{igrf12.f} subroutine, which was written by Susan Macmillan of the
#' British Geological Survey and distributed ``without limitation'' (email from
#' SM to DK dated June 5, 2015).
#' @references
#' 1. The underlying Fortran code is from \code{igrf12.f}, downloaded the NOAA
#' website (\url{https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}) on June 7,
#' 2015.
#'
#' 2. Witze, Alexandra. “Earth’s Magnetic Field Is Acting up and Geologists Don’t Know Why.”
#' Nature 565 (January 9, 2019): 143.
#' \url{https://doi.org/10.1038/d41586-019-00007-1}.
#"
#' @examples
#' library(oce)
#' # Halifax NS
#' magneticField(-(63+36/60), 44+39/60, 2013)
#'
#' \dontrun{
#' ## map of North American values
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
#'         projection="+proj=lcc +lat_0=20 +lat_1=60 +lon_0=-100")
#' lon <- seq(-180, 180, 1)
#' lat <- seq(-90, 90)
#' lonm <- rep(lon, each=length(lat))
#' latm <- rep(lat, times=length(lon))
#' ## Note the counter-intuitive nrow argument
#' decl <- matrix(magneticField(lonm, latm, 2013)$declination,
#'                nrow=length(lon), byrow=TRUE)
#' mapContour(lon, lat, decl, col='red', levels=seq(-90, 90, 5))
#' incl <- matrix(magneticField(lonm, latm, 2013)$inclination,
#'                nrow=length(lon), byrow=TRUE)
#' mapContour(lon, lat, incl, col='blue', levels=seq(-90, 90, 5))
#' }
#'
#' @family things related to magnetism
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


#' Locate byte sequences in a raw vector
#'
#' Find spots in a raw vector that match a given byte sequence.
#'
#' @param input a vector of raw (byte) values.
#' @param b1 a vector of bytes to match (must be of length 2 or 3 at present;
#' for 1-byte, use \code{\link{which}}).
#' @param \dots additional bytes to match for (up to 2 permitted)
#' @return List of the indices of \code{input} that match the start of the
#' \code{bytes} sequence (see example).
#' @author Dan Kelley
#' @examples
#'
#' buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
#' match <- matchBytes(buf, 0xa5, 0x11)
#' print(buf)
#' print(match)
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
        m <- m[c(seq.int(cut+1L, n), seq.int(1L, cut)), ]
    }
    list(m=m, longitude=longitude)
}


#' Smooth a Matrix
#'
#' The values on the edge of the matrix are unaltered.  For interior points,
#' the result is defined in terms in terms of the original as follows.
#' \eqn{r_{i,j} = (2 m_{i,j} + m_{i-1,j} + m_{i+1,j} + m_{i,j-1} +
#' m_{i,j+1})/6}{r_[i,j] = (2 m_[i,j] + m_[i-1,j] + m_[i+1,j] + m_[i,j-1] +
#' m_[i,j+1])/6}.  Note that missing values propagate to neighbours.
#'
#' @param m a matrix to be smoothed.
#' @param passes an integer specifying the number of times the smoothing is to
#' be applied.
#' @return A smoothed matrix.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' opar <- par(no.readonly = TRUE)
#' m <- matrix(rep(seq(0, 1, length.out=5), 5), nrow=5, byrow=TRUE)
#' m[3, 3] <- 2
#' m1 <- matrixSmooth(m)
#' m2 <- matrixSmooth(m1)
#' m3 <- matrixSmooth(m2)
#' par(mfrow=c(2, 2))
#' image(m,  col=rainbow(100), zlim=c(0, 4), main="original image")
#' image(m1, col=rainbow(100), zlim=c(0, 4), main="smoothed 1 time")
#' image(m2, col=rainbow(100), zlim=c(0, 4), main="smoothed 2 times")
#' image(m3, col=rainbow(100), zlim=c(0, 4), main="smoothed 3 times")
#' par(opar)
matrixSmooth <- function(m, passes=1)
{
    if (missing(m))
        stop("must provide matrix 'm'")
    storage.mode(m) <- "double"
    if (passes > 0) {
        for (pass in seq.int(1, passes, 1)) {
            m <- do_matrix_smooth(m)
        }
    } else {
        warning("matrixSmooth given passes<=0, so returning matrix unmodified")
    }
    m
}


#' Time interval as colon-separated string
#'
#' Convert a time interval to a colon-separated string
#'
#' @param sec length of time interval in seconds.
#' @return A string with a colon-separated time interval.
#' @author Dan Kelley
#' @seealso See \code{\link{ctimeToSeconds}}, the inverse of this.
#' @examples
#'
#' library(oce)
#' cat("   10 s = ", secondsToCtime(10), "\n", sep="")
#' cat("   61 s = ", secondsToCtime(61), "\n", sep="")
#' cat("86400 s = ", secondsToCtime(86400), "\n", sep="")
#' @family things related to time
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


#' Interpret a character string as a time interval
#'
#' Interpret a character string as a time interval
#' Strings are of the form MM:SS or HH:MM:SS.
#'
#' @param ctime a character string (see \sQuote{Details}.
#' @return A numeric value, the number of seconds represented by the string.
#' @author Dan Kelley
#' @seealso See \code{\link{secondsToCtime}}, the inverse of this.
#' @examples
#'
#' library(oce)
#' cat("10      = ", ctimeToSeconds("10"), "s\n", sep="")
#' cat("01:04   = ", ctimeToSeconds("01:04"), "s\n", sep="")
#' cat("1:00:00 = ", ctimeToSeconds("1:00:00"), "s\n", sep="")
#' @family things related to time
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



#' Print a debugging message
#'
#' Print an indented debugging message.
#' Many oce functions decrease the \code{debug} level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level.
#'
#' @aliases oce.debug
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#' @param \dots items to be supplied to \code{\link{cat}}, which does the
#' printing.  Almost always, this should include a trailing newline.
#' @param unindent Number of levels to un-indent, e.g. for start and end lines
#' from a called function.
#' @author Dan Kelley
#' @examples
#'
#' foo <- function(debug)
#' {
#'    oceDebug(debug, "in function foo\n")
#' }
#' debug <- 1
#' oceDebug(debug, "in main")
#' foo(debug=debug-1)
oceDebug <- function(debug=0, ..., unindent=0)
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
oce.debug <- oceDebug


#' Show metadata item
#'
#' This is a helper function for various \code{summary} functions.
#'
#' @param object an object inheriting from the base \code{oce} class.
#' @param name name of item
#' @param label label to print before item
#' @param postlabel label to print after item
#' @param isdate boolean indicating whether the item is a time
#' @param quote boolean indicating whether to enclose the item in quotes
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(ctd)
#' showMetadataItem(ctd, "ship", "ship")
showMetadataItem <- function(object, name, label="", postlabel="", isdate=FALSE, quote=FALSE)
{
    if (name %in% names(object@metadata)) {
        item <- object@metadata[[name]]
        if (is.null(item))
            return()
        if (is.na(item))
            return()
        if (is.character(item) && nchar(item) == 0)
            return()
        if (is.na(item))
            return()
        if (isdate) item <- format(item)
        if (quote) item <- paste('"', item, '"', sep="")
        cat(paste("* ", label, item, postlabel, "\n", sep=""))
    }
}


#' Trapezoidal Integration
#'
#' Estimate the integral of one-dimensional function using the trapezoidal
#' rule.
#'
#' @param x,y vectors of x and y values. In the normal case, these
#' vectors are both supplied, and of equal length. There are also two
#' special cases. First, if \code{y} is missing, then
#' \code{x} is taken to be \code{y}, and a new \code{x} is constructed
#' as \code{\link{seq_along}(y)}. Second, if \code{length(x)} is 1
#' and \code{length(y)} exceeds 1, then \code{x} is replaced by
#' \code{x*seq_along(y)}.
#' @param type Flag indicating the desired return value (see \dQuote{Value}).
#' @param xmin,xmax Optional numbers indicating the range of the integration.
#' These values may be used to restrict the range of integration, or to
#' extend it; in either case, \code{\link{approx}} with \code{rule=2}
#' is used to create new x and y vectors.
#'
#' @return If \code{type="A"} (the default), a single value is returned,
#' containing the estimate of the integral of \code{y=y(x)}.  If
#' \code{type="dA"}, a numeric vector of the same length as \code{x}, of which
#' the first element is zero, the second element is the integral between
#' \code{x[1]} and \code{x[2]}, etc.  If \code{type="cA"}, the result is the
#' cumulative sum (as in \code{\link{cumsum}}) of the values that would be
#' returned for \code{type="dA"}.  See \dQuote{Examples}.
#' @section Bugs: There is no handling of \code{NA} values.
#' @author Dan Kelley
#' @examples
#' x <- seq(0, 1, length.out=10) # try larger length.out to see if area approaches 2
#' y <- 2*x + 3*x^2
#' A <- integrateTrapezoid(x, y)
#' dA <- integrateTrapezoid(x, y, "dA")
#' cA <- integrateTrapezoid(x, y, "cA")
#' print(A)
#' print(sum(dA))
#' print(tail(cA, 1))
#' print(integrateTrapezoid(diff(x[1:2]), y))
#' print(integrateTrapezoid(y))
integrateTrapezoid <- function(x, y, type=c("A", "dA", "cA"), xmin, xmax)
{
    type <- match.arg(type)
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) {
        y <- x
        x <- seq_along(y)
    }
    if (length(x) == 1 && length(y) > 1)
       x <- x * seq_along(y)
    if (length(x) != length(y))
        stop("'x' and 'y' must be of same length")
    xout <- x
    yout <- y
    ## message("x: ", paste(x, collapse=" "))
    ## message("y: ", paste(y, collapse=" "))
    if (!missing(xmin) || !missing(xmax)) {
        if (missing(xmin) || missing(xmax))
            stop("'xmin' and 'xmax' must both be supplied, if either is")
        if (xmin >= xmax)
            stop("'xmin' must be less than 'xmax'")

        if (xmin > max(x, na.rm=TRUE))
            stop("xmin must be less than max(x)")
        if (xmin < min(x, na.rm=TRUE)) {
            xout <- c(xmin, xout)
        } else {
            xout <- xout[xout >= xmin]
            xout <- c(xmin, xout)
        }

        if (xmax < min(x, na.rm=TRUE))
            stop("xmax must be greater than min(x)")
        if (xmax > max(x, na.rm=TRUE)) {
            xout <- c(xout, xmax)
        } else {
            xout <- xout[xout < xmax]
            xout <- c(xout, xmax)
        }
        yout <- approx(x, y, xout, rule=2)$y
    }
    ## message("\nabout to .Call(\"trap\", xout, yout, ...) with:\n")
    ## message("xout as follows:\n", paste(head(xout, 10), collapse="\n"))
    ## message("yout as follows:\n", paste(head(yout, 10), collapse="\n"))
    ##:::res <- .Call("trap", xout, yout, as.integer(switch(match.arg(type), A=0, dA=1, cA=2)))
    ##:::res <- trap(x=xout, y=yout, type=as.integer(switch(match.arg(type), A=0, dA=1, cA=2)))
    ##
    ## I think we should be able to use trap(), which gets defined into
    ## R/RcppExports.R but that doesn't seem to be put into the loadspace.
    ## My guess is that the problem is because we are not doing exports in
    ## the recommended (automatic) way, but I don't want to do exports that
    ## way since things are ok now, and have been for years.
    ## I don't see much point trying to figure this out, because we already
    ## have things set up for a .Call() from before the switch from C to Cpp.
    ##
    ## NOTE: must run Rcpp::compileAttributes() after creating trap in
    ## src/trap.cpp
    res <- do_trap(xout, yout, as.integer(switch(match.arg(type), A=0, dA=1, cA=2)))
    ##> res <- trap( xout, yout, as.integer(switch(match.arg(type), A=0, dA=1, cA=2)))
    res
}


#' Calculate Matrix Gradient
#'
#' In the interior of the matrix, centred second-order differences are used to
#' infer the components of the grad.  Along the edges, first-order differences
#' are used.
#'
#' @param h a matrix of values
#' @param x vector of coordinates along matrix columns (defaults to integers)
#' @param y vector of coordinates along matrix rows (defaults to integers)
#' @return A list containing \eqn{|\nabla h|}{abs(grad(h))} as \code{g},
#' \eqn{\partial h/\partial x}{dh/dx} as \code{gx},
#' and \eqn{\partial h/\partial y}{dh/dy} as \code{gy},
#' each of which is a matrix of the same dimension as \code{h}.
#' @author Dan Kelley, based on advice of Clark Richards, and mimicking a matlab function.
#' @examples
#' ## 1. Built-in volcano dataset
#' g <- grad(volcano)
#' par(mfrow=c(2, 2), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' imagep(volcano, zlab="h")
#' imagep(g$g, zlab="|grad(h)|")
#' zlim <- c(-1, 1) * max(g$g)
#' imagep(g$gx, zlab="dh/dx", zlim=zlim)
#' imagep(g$gy, zlab="dh/dy", zlim=zlim)
#'
#' ## 2. Geostrophic flow around an eddy
#' library(oce)
#' dx <- 5e3
#' dy <- 10e3
#' x <- seq(-200e3, 200e3, dx)
#' y <- seq(-200e3, 200e3, dy)
#' R <- 100e3
#' h <- outer(x, y, function(x, y) 500*exp(-(x^2+y^2)/R^2))
#' grad <- grad(h, x, y)
#' par(mfrow=c(2, 2), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' contour(x,y,h,asp=1, main=expression(h))
#' f <- 1e-4
#' gprime <- 9.8 * 1 / 1024
#' u <- -(gprime / f) * grad$gy
#' v <-  (gprime / f) * grad$gx
#' contour(x, y, u, asp=1, main=expression(u))
#' contour(x, y, v, asp=1, main=expression(v))
#' contour(x, y, sqrt(u^2+v^2), asp=1, main=expression(speed))
#' @family functions relating to vector calculus
grad <- function(h, x=seq(0, 1, length.out=nrow(h)), y=seq(0, 1, length.out=ncol(h)))
{
    if (missing(h))
        stop("must give h")
    if (length(x) != nrow(h))
        stop("length of x (", length(x), ") must equal number of rows in h (", nrow(h), ")")
    if (length(y) != ncol(h))
        stop("length of y (", length(y), ") must equal number of cols in h (", ncol(h), ")")
    ## ensure that all three args are double, so the C code won't misinterpret
    dim <- dim(h)
    h <- as.double(h)
    dim(h) <- dim
    x <- as.double(x)
    y <- as.double(y)
    rval <- do_gradient(h, x, y)
    rval$g <- sqrt(rval$gx^2 + rval$gy^2)
    rval
}


#' Version of as.raw() that clips data
#'
#' A version of as.raw() that clips data to prevent warnings
#'
#' Negative values are clipped to 0, while values above 255 are clipped to 255;
#' the result is passed to \code{\link{as.raw}} and returned.
#'
#' @param x values to be converted to raw
#' @return Raw values corresponding to \code{x}.
#' @author Dan Kelley
#' @examples
#'
#' x <- c(-0.1, 0, 1, 255, 255.1)
#' data.frame(x, oce.as.raw(x))
oce.as.raw <- function(x)
{
    ## prevent warnings from out-of-range with as.raw()
    na <- is.na(x)
    x[na] <- 0                 # FIXME: what to do here?
    x <- ifelse(x < 0, 0, x)
    x <- ifelse(x > 255, 255, x)
    x <- as.raw(x)
    x
}


#' Convolve two time series
#'
#' Convolve two time series, using a backward-looking method.
#' This function provides a straightforward convolution, which may be useful to
#' those who prefer not to use \code{\link{convolve}} and \code{filter} in the
#' \code{stats} package.
#'
#' @aliases oce.convolve
#' @param x a numerical vector of observations.
#' @param f a numerical vector of filter coefficients.
#' @param end a flag that controls how to handle the points of the \code{x}
#' series that have indices less than the length of \code{f}.  If \code{end=0},
#' the values are set to 0.  If \code{end=1}, the original x values are used
#' there.  If \code{end=2}, that fraction of the \code{f} values that overlap
#' with \code{x} are used.
#' @return A vector of the convolution output.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' t <- 0:1027
#' n <- length(t)
#' signal <- ifelse(sin(t * 2 * pi / 128) > 0, 1, 0)
#' tau <- 10
#' filter <- exp(-seq(5*tau, 0) / tau)
#' filter <- filter / sum(filter)
#' observation <- oce.convolve(signal, filter)
#' plot(t, signal, type='l')
#' lines(t, observation, lty='dotted')
#'
oceConvolve <- function(x, f, end=2)
{
    do_oce_convolve(x, f, end)
}
oce.convolve <- oceConvolve


#' Remove leading and trailing whitespace from strings
#'
#' @param s vector of character strings
#' @return a new vector formed by trimming leading and trailing whitespace
#' from the elements of \code{s}.
trimString <- function(s)
{
    gsub("^ *", "", gsub(" *$", "", s))
}

#' Perform lowpass digital filtering
#'
#' The filter coefficients are constructed using standard definitions,
#' and then \link[stats]{filter} in the \pkg{stats} package is
#' used to filter the data. This leaves \code{NA}
#' values within half the filter length of the ends of the time series, but
#' these may be replaced with the original \code{x} values, if the argument
#' \code{replace} is set to \code{TRUE}.
#'
#' @section Caution: This function was added in June of 2017,
#' and it may be extended during the rest of 2017. New arguments
#' may appear between \code{n} and \code{replace}, so users are
#' advised to call this function with named arguments, not positional
#' arguments.
#'
#' @param x a vector to be smoothed
#' @param filter name of filter; at present, \code{"hamming"}, \code{"hanning"}, and \code{"boxcar"} are permitted.
#' @param n length of filter (must be an odd integer exceeding 1)
#' @param replace a logical value indicating whether points near the
#' ends of \code{x} should be copied into the end regions, replacing
#' the \code{NA} values that would otherwise be placed there by
#' \link[stats]{filter}.
#' @param coefficients logical value indicating whether to return
#' the filter coefficients, instead of the filtered values. In accordance
#' with conventions in the literature, the returned values are not
#' normalized to sum to 1, although of course that normalization
#' is done in the actual filtering.
#'
#' @return By default, \code{lowpass} returns a filtered version
#' of \code{x}, but if \code{coefficients} is \code{TRUE} then it
#' returns the filter coefficients.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' par(mfrow=c(1, 2), mar=c(4, 4, 1, 1))
#' coef <- lowpass(n=5, coefficients=TRUE)
#' plot(-2:2, coef, ylim=c(0, 1), xlab="Lag", ylab="Coefficient")
#' x <- seq(-5, 5) + rnorm(11)
#' plot(1:11, x, type='o', xlab="time", ylab="x and X")
#' X <- lowpass(x, n=5)
#' lines(1:11, X, col=2)
#' points(1:11, X, col=2)
lowpass <- function(x, filter="hamming", n, replace=TRUE, coefficients=FALSE)
{
    # .Call("hammingFilter", x, n)
    if (missing(x) && !coefficients)
        stop("must supply x")
    if (missing(n))
        stop("must supply n")
    if (n < 1)
        stop("n must be be an integer exceeding 1")
    n2 <- n %/% 2 # half width
    if (2 * n2 == n)
        stop("n must be an odd integer")
    twopi <- 8 * atan2(1, 1)
    ii <- n2 + seq.int(-n2, n2, 1)
    if (filter == "hamming")
        f <- 0.54 - 0.46 * cos(twopi * ii / (n - 1))
    else if (filter == "hanning")
        f <- 0.5 * (1 - cos(twopi * ii / (n - 1)))
    else if (filter == "boxcar")
        f <- rep(1/n, n)
    else
        stop("filter must be \"hanning\", \"hamming\", or \"boxcar\"")
    if (coefficients) {
        rval <- f
    } else {
        f <- f / sum(f)
        rval <- as.numeric(stats::filter(x=x, filter=f, method="convolution"))
        if (replace) {
            start <- seq.int(1, n2)
            rval[start] <- x[start]
            nx <- length(x)
            end <- seq.int(nx-n2+1, nx)
            rval[end] <- x[end]
        }
    }
    rval
}

## unalphabetized functions END
