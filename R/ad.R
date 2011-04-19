## functions that dispatch to either adv or adp
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
        xyzToEnuAdp(x, ...)
    else if (inherits(x, "adv"))
        xyzToEnuAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

enuToOther <- function(x, ...)
{
    if (inherits(x, "adp"))
        enuToOther(x, ...)
    else if (inherits(x, "adv"))
        enuToOther(x, ...)
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

