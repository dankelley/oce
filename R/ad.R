## functions that dispatch to either adv or adp
velocityStatistics <- function(x, control, ...)
{
    if (inherits(x, "adp")) {
        if (!missing(control) && !is.null(control$bin)) {
            if (control$bin < 1)
                stop("cannot have control$bin less than 1, but got ", control$bin)
            max.bin <- dim(x@data$v)[2]
            if (control$bin > max.bin)
                stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
            u <- x@data$v[,control$bin,1]
            v <- x@data$v[,control$bin,2]
        } else {
            u <- apply(x@data$v[,,1], 1, mean, na.rm=TRUE) # depth mean
            v <- apply(x@data$v[,,2], 1, mean, na.rm=TRUE) # depth mean
        }
    } else if (inherits(x, "adv")) {
        u <- x@data$v[, 1]
        v <- x@data$v[, 2]
    }
    ok <- !is.na(u) & !is.na(v)
    u <- u[ok]
    v <- v[ok]
    e <- eigen(cov(data.frame(u, v)))
    ellipseMajor <- sqrt(e$values[1])
    ellipseMinor <- sqrt(e$values[2])
    ellipseAngle <- atan2(e$vectors[2,1], e$vectors[1,1]) * 45 / atan2(1, 1)
    uMean <- mean(u, ...)
    vMean <- mean(v, ...)
    list(ellipseMajor=ellipseMajor, ellipseMinor=ellipseMinor, ellipseAngle=ellipseAngle,
         uMean=uMean, vMean=vMean)
}

beamToXyz <- function(x, ...)
{
    if (inherits(x, "adp"))
        beamToXyzAdp(x, ...)
    else if (inherits(x, "adv"))
        beamToXyzAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

xyzToEnu <- function(x, ...)
{
    if (inherits(x, "adp"))
        xyzToEnuAdp(x=x, ...)
    else if (inherits(x, "adv"))
        xyzToEnuAdv(x=x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

enuToOther <- function(x, ...)
{
    if (inherits(x, "adp"))
        enuToOtherAdp(x, ...)
    else if (inherits(x, "adv"))
        enuToOtherAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

toEnu <- function(x, ...)
{
    if (inherits(x, "adp"))
        toEnuAdp(x, ...)
    else if (inherits(x, "adv"))
        toEnuAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

