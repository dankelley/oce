#' Look within the first element of a list, replacing other elements from its contents.
#'
#' @details 
#' This is a helper function used by various seawater functions. It is used for a
#' call like \code{\link{swRho}(ctd)}, in which the first argument, which is
#' normally \code{salinity} may be an object that contains salinity plus
#' the other items that \code{\link{swRho}} expects to see as arguments. This
#' shorthand is very helpful in calls to the suite of \code{sw} functions.  If
#' this first argument is an object of this sort, then the other arguments 
#' are ignored \emph{except} for one named \code{eos}, which is copied if
#' it is present.
#'
#' @param list A list of elements, typically arguments that will be used in sw functions.
#' @return A list with elements of the same names but possibly filled in from the first element.
lookWithin <- function(list)
{
    n <- length(list)
    names <- names(list)
    ## str(list)
    list1 <- list[[1]]
    if (inherits(list[[1]], "oce")) {
        for (i in 1:n) {
            if ("eos" != names[i]) {
                try({
                    list[[i]] <- list1[[names[i], "nowarn"]]
                }, silent=TRUE)
            }
        }
        if (inherits(list1, "ctd")) {
            nS <- length(list[[names[1]]])
            list[["longitude"]] <- rep(list[["longitude"]][1], nS)
            list[["latitude"]] <- rep(list[["latitude"]][1], nS)
        }
        ## FIXME: should special-case some other object types
    }
    if ("eos" %in% names)
        list[["eos"]] <- match.arg(list[["eos"]], c("unesco", "gsw"))
    list
}


swRrho <- function(ctd, sense=c("diffusive", "finger"), smoothingLength=10, df)
{
    if (!inherits(ctd, "ctd"))
        stop("first argument must be of class \"ctd\"")
    sense <- match.arg(sense)
    pressure <- ctd[['pressure']]
    salinity <- ctd[['salinity']]
    theta <- ctd[['theta']]
    if (length(pressure) < 4)
        return(NA)
    alpha <- swAlpha(ctd)
    beta <- swBeta(ctd)
    A <- smoothingLength / mean(diff(pressure), na.rm=TRUE) 
    n <- length(pressure)
    ## infer d(theta)/dp and d(salinity)/dp from smoothing splines
    if (missing(df)) {
        thetaSpline <- smooth.spline(pressure, theta, df=n/A)
        salinitySpline <- smooth.spline(pressure, salinity, df=n/A)
    } else {
        thetaSpline <- smooth.spline(pressure, theta, df=df)
        salinitySpline <- smooth.spline(pressure, salinity, df=df)
    }
    dthetadp <- predict(thetaSpline, pressure, deriv=1)$y
    dsalinitydp <- predict(salinitySpline, pressure, deriv=1)$y
    if (sense == "diffusive")
        Rrho <- (beta * dsalinitydp)/ (alpha * dthetadp)
    else
        Rrho <- (alpha * dthetadp) / (beta * dsalinitydp)
    Rrho
}

swN2 <- function(pressure, sigmaTheta=NULL, derivs, df, ...)
{
    ##cat("swN2(..., df=", df, ")\n",sep="")
    if (inherits(pressure, "ctd")) {
        sigmaTheta <- swSigmaTheta(pressure)
        pressure <- pressure[['pressure']] # over-writes pressure
    }
    if (missing(derivs))
        derivs <- "smoothing"
    ok <- !is.na(pressure) & !is.na(sigmaTheta)
    if (!missing(df)) {
        df <- max(2, min(df, length(unique(pressure))))
    }
    if (is.character(derivs)) {
        if (derivs == "simple") {
            sigmaThetaDeriv <- c(0, diff(sigmaTheta) / diff(pressure))
        } else if (derivs == "smoothing") {
            args <- list(...)
            depths <- length(pressure)
            ##df <- if (is.null(args$df)) min(floor(length(pressure)/5), 10) else args$df;
            if (missing(df)) {
                if (depths > 20)
                    df <- floor(depths / 10)
                else if (depths > 10)
                    df <- floor(depths / 3)
                else
                    df <- floor(depths / 2)
                oceDebug(getOption("oceDebug"), "df not supplied, so set to ", df, "(note: #depths=", depths, ")\n")
            }
            df <- min(df, length(unique(pressure))/2)
            if (depths > 4 && df > 1) {
                sigmaThetaSmooth <- smooth.spline(pressure[ok], sigmaTheta[ok], df=df)
                sigmaThetaDeriv <- rep(NA, length(pressure))
                sigmaThetaDeriv[ok] <- predict(sigmaThetaSmooth, pressure[ok], deriv = 1)$y
            } else {
                sigmaThetaSmooth <- as.numeric(smooth(sigmaTheta[ok]))
                sigmaThetaDeriv <- rep(NA, length(pressure))
                sigmaThetaDeriv[ok] <- c(0, diff(sigmaThetaSmooth) / diff(pressure[ok]))
            }
        } else {
            stop("derivs must be 'simple', 'smoothing', or a function")
        }
    } else {
        if (!is.function(derivs))
            stop("derivs must be 'smoothing', 'simple', or a function")
        sigmaThetaDeriv <- derivs(pressure, sigmaTheta)
    }
    ifelse(ok, 9.8 * 9.8 * 1e-4 * sigmaThetaDeriv, NA)
}

swPressure <- function(depth, latitude=45)
{
    ## FIXME-gsw add gsw version
    ndepth <- length(depth)
    if (length(latitude) < ndepth)
        latitude <- rep(latitude, ndepth)
    rval <- vector("numeric", ndepth)
    for (i in 1:ndepth) {
        rval[i] <- uniroot(function(p) depth[i] - swDepth(p, latitude[i]), interval=depth[i]*c(0.8, 1.2))$root
    }
    rval
}

swSCTp <- function(conductivity, temperature, pressure, conductivityUnit=c("ratio", "mS/cm", "S/m"))
{
    ## FIXME-gsw add gsw version
     if (missing(conductivity) || missing(temperature))
        stop("must supply conductivity, temperature and pressure")
    conductivityUnit <- match.arg(conductivityUnit)
    if (conductivityUnit == "mS/cm")
        conductivity <- conductivity / 42.914
    else if (conductivityUnit == "S/m")
        conductivity <- conductivity / 4.2914
    dim <- dim(conductivity)
    nC <- length(conductivity)
    nT <- length(temperature)
    if (nC != nT)
        stop("lengths of C and temperature must agree, but they are ", nC, " and ", nT, ", respectively")
    if (missing(pressure))
        pressure <- rep(0, nC)
    np <- length(pressure)
    if (nC != np)
        stop("lengths of C and p must agree, but they are ", nC, " and ", np, ", respectively")
    rval <- .C("sw_salinity",
               as.integer(nC),
               as.double(conductivity),
               as.double(temperature),
               as.double(pressure),
               value = double(nC),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

## FIXME-speed: should be vectorized
swSTrho <- function(temperature, density, pressure, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw add gsw version
    eos <- match.arg(eos, c("unesco", "gsw"))
    teos <- eos == "gsw" # FIXME still the best way?
    dim <- dim(temperature)
    nt <- length(temperature)
    nrho <- length(density)
    np <- length(pressure)
    if (nt != nrho)
        stop("lengths of temperature and density must agree, but they are ", nt, " and ", nrho, ", respectively")
    if (nt != np)
        stop("lengths of temperature and p arrays must agree, but they are ", nt, " and ", np, ", respectively")
    for (i in 1:nt) {                   # FIXME: avoid loops
        sigma <- ifelse(density > 500, density - 1000, density)
    	this.S <- .C("sw_strho",
                     as.double(temperature[i]), # FIXME: confusion on "p" here; and is temp theta??
                     as.double(sigma),
                     as.double(pressure[i]),
                     as.integer(teos),
                     S = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$S
    	if (i == 1) rval <- this.S else rval <- c(rval, this.S)
    }
    dim(rval) <- dim
    rval
}

## FIXME: should be vectorized
swTSrho <- function(salinity, density, pressure=NULL, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw add gsw version
    if (missing(salinity))
        stop("must provide salinity")
    eos <- match.arg(eos, c("unesco", "gsw"))
    teos <- eos == "gsw"
    dim <- dim(salinity)
    nS <- length(salinity)
    nrho <- length(density)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    if (length(pressure) == 1)
        pressure <- rep(pressure[1], length.out=nS)
    np <- length(pressure)
    if (nS != nrho)
        stop("lengths of salinity and rho must agree, but they are ", nS, " and ", nrho,  ", respectively")
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    for (i in 1:nS) {                   # FIXME: avoid loops
    	sig <- density[i]
    	if (sig > 500) {
            sig <- sig - 1000
    	}
        ## FIXME: is this right for all equations of state? I doubt it
    	this.T <- .C("sw_tsrho",
                     as.double(salinity[i]),
                     as.double(sig),
                     as.double(pressure[i]),
                     as.integer(teos),
                     temperature = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$t
    	if (i == 1) rval <- this.T else rval <- c(rval, this.T)
    }
    dim(rval) <- dim
    rval
}

swTFreeze <- function(salinity, pressure=0,
                      longitude=300, latitude=30, saturation_fraction=1,
                      eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    n <- length(l$salinity)
    if (eos == "unesco") {
        rval <- (-.0575+1.710523e-3*sqrt(abs(l$salinity))-2.154996e-4*l$salinity)*l$salinity-7.53e-4*l$pressure
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        rval <- gsw_t_freezing(SA=SA, p=l$pressure, saturation_fraction=1)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swAlpha <- function(salinity, temperature=NULL, pressure=NULL,
                    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version
    eos <- match.arg(eos, c("unesco", "gsw"))
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, 
                         longitude=longitude, latitude=latitude, eos=eos))
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) l$pressure <- 0
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (l$eos == "unesco") {
        swAlphaOverBeta(l$salinity, l$temperature, l$pressure) * swBeta(l$salinity, l$temperature, l$pressure)
    } else {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        rval <- gsw_alpha(SA=SA, CT=CT, p=l$pressure)
    }
}

swAlphaOverBeta <- function(salinity, temperature=NULL, pressure=NULL,
                   longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    if (is.null(l$temperature)) stop("must provide temperature")
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) pressure <- 0
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        t <- swTheta(l$salinity, l$temperature, l$pressure)
        rval <- .C("sw_alpha_over_beta", as.integer(nS),
                   as.double(l$salinity), as.double(t), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        ## not likely to be called since gsw has a direct function for alpha, but put this here anyway
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        alpha <- gsw_alpha(SA=SA, CT=CT, p=l$pressure)
        beta <- gsw_beta(SA=SA, CT=CT, p=l$pressure)
        rval <- alpha / beta
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swBeta <- function(salinity, temperature=NULL, pressure=NULL,
                   longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) pressure <- 0
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        theta <- swTheta(l$salinity, l$temperature, l$pressure) # the formula is i.t.o. theta
        rval <- .C("sw_beta", as.integer(nS),
                   as.double(l$salinity), as.double(theta), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        rval <- gsw_beta(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swConductivity <- function (salinity, temperature=NULL, pressure=NULL)
{
    ## FIXME-gsw need a gsw version
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure))
    return(0.57057 * (1 + l$temperature * (0.003 - 1.025e-05 * l$temperature) + 0.000653 * l$pressure - 0.00029 * l$salinity))
}

swDepth <- function(pressure, latitude=45, degrees=TRUE)
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure)) stop("must provide pressure")
    l <- lookWithin(list(pressure=pressure, latitude=latitude))
    if (degrees) l$latitude <- l$latitude * 0.0174532925199433
    x <- sin(l$latitude)^2
    gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*l$pressure
    (((-1.82e-15*l$pressure+2.279e-10)*l$pressure-2.2512e-5)*l$pressure+9.72659)*l$pressure / gr
}

swZ <- function(pressure, latitude=45, degrees=TRUE)
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure)) stop("must provide pressure")
    -swDepth(pressure=pressure, latitude=latitude, degrees=degrees)
}

swDynamicHeight <- function(x, referencePressure=2000,
                            subdivisions=500, rel.tol=.Machine$double.eps^0.25)
{
    height <- function(ctd, referencePressure, subdivisions, rel.tol)
    {
        if (sum(!is.na(ctd@data$pressure)) < 2) return(NA) # cannot integrate then
        g <- if (is.na(ctd@metadata$latitude)) 9.8 else gravity(ctd@metadata$latitude)
        np <- length(ctd@data$pressure)
        rho <- swRho(ctd)
        if (sum(!is.na(rho)) < 2) return(NA)
        ## 1e4 converts decibar to Pa
        dzdp <- ((1/rho - 1/swRho(rep(35,np),rep(0,np),ctd@data$pressure))/g)*1e4
        ## Scale both pressure and dz/dp to make integration work better (issue 499)
        max <- max(dzdp, na.rm=TRUE)
        integrand <- approxfun(ctd@data$pressure/referencePressure, dzdp/max, rule=2)
        ##plot(dzdp/max, ctd@data$pressure/referencePressure, type='l')
        integrate(integrand, 0, 1,
                  subdivisions=subdivisions, rel.tol=rel.tol)$value * referencePressure * max
    }
    if (inherits(x, "section")) {
        lon0 <- x@data$station[[1]]@metadata$longitude
        lat0 <- x@data$station[[1]]@metadata$latitude
        ns <- length(x@data$station)
        d <- vector("numeric", ns)
        h <- vector("numeric", ns)
        for (i in 1:ns) {
            d[i] <- geodDist(x@data$station[[i]]@metadata$longitude, x@data$station[[i]]@metadata$latitude, lon0, lat0)
            h[i] <- height(x@data$station[[i]], referencePressure, subdivisions=subdivisions, rel.tol=rel.tol)
        }
        return(list(distance=d, height=h))
    } else if (inherits(x, "ctd")) {
        return(height(x, referencePressure, subdivisions=subdivisions, rel.tol=rel.tol))
    } else {
        stop("method is only for objects of class '", "section", " or '", "ctd", "'")
    }
}

swLapseRate <- function(salinity, temperature=NULL, pressure=NULL,
                        longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    if (is.null(l$temperature)) stop("must provide temperature")
    nt <- length(l$temperature)
    if (is.null(l$pressure)) l$pressure <- rep(0, length.out=nS)
    if (length(l$pressure) == 1)
        l$pressure <- rep(l$pressure[1], length.out=nS)
    np <- length(l$pressure)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        rval <- .C("sw_lapserate", as.integer(nS), as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        ## the 1e4 is to convert from 1/Pa to 1/dbar
        rval<- 1e4 * gsw_adiabatic_lapse_rate_from_CT(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}                                      # swLapseRate 

swRho <- function(salinity, temperature=NULL, pressure=NULL,
                  longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    if (is.null(l$temperature)) stop("must provide temperature")
    if (is.null(l$pressure)) stop("must provide pressure")
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) l$pressure <- 0
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        rval <- .C("sw_rho", as.integer(nS), as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        rval <- gsw_rho(SA, CT, p=l$pressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swSigma <- function(salinity, temperature=NULL, pressure=NULL,
                    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    swRho(salinity, temperature, pressure,
          longitude=longitude, latitude=latitude, eos=eos) - 1000
}

swSigmaT <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    swRho(l$salinity, l$temperature, pressure=rep(0, length(l$salinity)),
          longitude=l$longitude, latitude=l$latitude, eos=l$eos) - 1000
}

swSigmaTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
                         longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    theta <- swTheta(salinity=l$salinity, temperature=l$temperature, pressure=l$pressure,
                     referencePressure=referencePressure,
                     longitude=l$longitude, latitude=l$latitude, eos=l$eos)
    swRho(salinity=l$salinity, temperature=theta, pressure=0,
          longitude=l$longitude, latitude=l$latitude, eos=l$eos) - 1000
}

swSigma0 <- function(salinity, temperature, pressure,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=0,
                 longitude=longitude, latitude=latitude, eos=eos)
}

swSigma1 <- function(salinity, temperature, pressure,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=1000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

swSigma2 <- function(salinity, temperature, pressure,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=2000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

swSigma3 <- function(salinity, temperature, pressure,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=3000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

swSigma4 <- function(salinity, temperature, pressure,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=4000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

swSoundAbsorption <- function(frequency, salinity, temperature, pressure, pH=8,
                              formulation=c("fisher-simmons", "francois-garrison"))
{
    formulation <- match.arg(formulation)
    if (formulation == "fisher-simmons") {
        ## Equation numbers are from Fisher & Simmons (1977); see help page for ref
        p <- 1 + pressure / 10  # add atmophere, then convert water part from dbar
        S <- salinity
        T <- temperature
        f <- frequency
        A1 <- 1.03e-8 + 2.36e-10*T - 5.22e-12*T^2                       # (5)
        f1 <- 1.32e3*(T + 273.1)*exp(-1700/(T + 273.1))                 # (6)
        A2 <- 5.62e-8 + 7.52e-10 * T                                    # (7)
        f2 <- 1.55e7 * (T + 273.1)*exp(-3052/(T + 273.1))               # (8)
        P2 <- 1 - 10.3e-4 * p + 3.7e-7 * p^2                            # (9)
        A3 <-(55.9 - 2.37 * T + 4.77e-2 * T^2  - 3.48e-4*T^3) * 1e-15   # (10)
        P3 <- 1 - 3.84e-4 * p + 7.57e-8 * p^2                           # (11)
        alpha <- (A1*f1*f^2)/(f1^2 + f^2) + (A2*P2*f2*f^2)/(f2^2 + f^2) + A3*P3*f^2 # (3a)
        alpha <- alpha * 8686 / 1000   # dB/m
    } else if (formulation == "francois-garrison") {
        S <- salinity
        T <- temperature
        D <- pressure # FIXME: approximation
        c <- 1412 + 3.21 * T + 1.19 * S + 0.0167 * D # sound speed m/s
        f <- frequency / 1000          # convert to kHz
        theta <- 273 + T 
        f1 <- 2.8 * sqrt(S / 35) * 10^(4 - 1245 / theta) # kHz
        ## subscript 1 for boric acid contribution
        A1 <- 8.86 / c * 10^(0.78 * pH - 5) # dB / km / kHz
        P1 <- 1
        ## MgSO4 contribution
        A2 <- 21.44 * (S / c) * (1 + 0.025 * T) # dB / km / kHz
        P2 <- 1 - 1.37e-4 * D + 6.2e-9 * D^2
        f2 <- (8.17 * 10^(8 - 1990 / theta)) / (1 + 0.0018 * (S - 35)) # kHz
        ## pure water contribution
        A3 <- 3.964e-4 - 1.146e-5 * T + 1.45e-7 * T^2 - 6.5e-10 * T^3 # dB / km / kHz^2
        P3 <- 1 - 3.83e-5 * D + 4.9e-10 * D^2
        alpha <- (A1 * P1 * f1 * f^2)/(f^2 + f1^2) + (A2 * P2 * f2 * f^2)/(f^2 + f2^2) + A3 * P3 * f^2
        alpha <- alpha / 1000
    }
    alpha
}

swSoundSpeed <- function(salinity, temperature=NULL, pressure=NULL,
                         longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    if (is.null(l$temperature)) stop("must provide temperature")
    if (is.null(l$pressure)) stop("must provide pressure")
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    np <- length(l$pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    l$pressure <- rep(l$pressure, length.out=nS)
    if (eos == "unesco") {
        rval <- .C("sw_svel", as.integer(nS), as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        rval <- gsw_sound_speed(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

## Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
## check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
swSpecificHeat <- function(salinity, temperature=NULL, pressure=0,
                           longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    if (is.null(l$temperature)) stop("must provide temperature")
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        rval <- .Fortran("cp_driver", as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
                         as.integer(nS), CP=double(nS))$CP
    } else {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        rval <- gsw_cp_t_exact(SA=SA, t=l$temperature, p=l$pressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swSpice <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure))
    if (is.null(l$temperature)) stop("must provide temperature")
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) l$pressure <- rep(0, nS)
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_spice", as.integer(nS), as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
               value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    if (Smatrix) dim(rval) <- dim
    rval
}

swTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
                    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    if (is.null(l$temperature)) stop("must provide temperature")
    if (is.null(l$pressure)) stop("must provide pressure")
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure))
        pressure <- 0
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    referencePressure <- rep(referencePressure[1], length.out=nS)
    if (eos == "unesco") {
        rval <- .C("theta_UNESCO_1983",
                   as.integer(nS), as.double(l$salinity), as.double(l$temperature), as.double(l$pressure),
                   as.double(l$referencePressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        rval <- gsw_pt_from_t(SA=SA, t=l$temperature, p=l$pressure, p_ref=referencePressure)
    }
    if (Smatrix) dim(rval) <- dim
    rval
}

swViscosity <- function(salinity, temperature=NULL)
{
    if (missing(salinity)) stop("must provide salinity")
    if (missing(temperature)) stop("must provide temperature")
    l <- lookWithin(list(salinity=salinity, temperature=temperature))
    0.001798525 + l$salinity * (2.634749e-06 - 7.088328e-10 *
           l$temperature^2 + l$salinity * (-4.702342e-09 + l$salinity *
           (5.32178e-11))) + l$temperature * (-6.293088e-05 +
           l$temperature * (1.716685e-06 + l$temperature * (-3.479273e-08
           + l$temperature * (+3.566255e-10))))
}

swConservativeTemperature <- function(salinity, temperature=NULL, pressure=NULL,
                                      longitude=300, latitude=30)
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    bad <- is.na(l$salinity) | is.na(l$temperature) | is.na(l$pressure)
    SA <- gsw_SA_from_SP(SP=l$salinity[!bad], p=l$pressure[!bad], 
                         longitude=l$longitude[!bad], latitude=l$latitude[!bad])
    good <- gsw_CT_from_t(SA=SA, t=l$temperature[!bad], p=l$pressure[!bad])
    rval <- rep(NA, nS)
    rval[!bad] <- good
    if (Smatrix) dim(rval) <- dim
    rval
}

swAbsoluteSalinity <- function(salinity, pressure=NULL, longitude=300, latitude=30)
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, pressure=pressure, longitude=longitude, latitude=latitude))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    bad <- is.na(l$salinity) | is.na(l$pressure) | is.na(l$longitude) | is.na(l$latitude)
    good <- gsw_SA_from_SP(l$salinity[!bad], l$pressure[!bad], l$longitude[!bad], l$latitude[!bad])
    rval <- rep(NA, nS)
    rval[!bad] <- good
    if (Smatrix) dim(rval) <- dim
    rval
}
