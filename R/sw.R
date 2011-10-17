swN2 <- function(pressure, sigmaTheta=NULL, derivs, ...) # BUG: think more about best density measure
{
    if (inherits(pressure, "ctd")) {
        if (!isS4(pressure))
            pressure <- makeS4(pressure)
        sigmaTheta <- swSigmaTheta(pressure@data$salinity, pressure@data$temperature, pressure@data$pressure)
        pressure <- pressure@data$pressure # over-writes pressure
    }
    ok <- !is.na(pressure) & !is.na(sigmaTheta)
    if (missing(derivs)) {
        args <- list(...)
        depths <- length(pressure)
        df <- if (is.null(args$df)) min(floor(length(pressure)/5), 10) else args$df;
        if (depths > 4 && df > 1) {
            sigmaThetaSmooth <- smooth.spline(pressure[ok], sigmaTheta[ok], df=df)
            sigmaThetaDeriv <- rep(NA, length(pressure))
            sigmaThetaDeriv[ok] <- predict(sigmaThetaSmooth, pressure[ok], deriv = 1)$y
        } else {
            sigmaThetaSmooth <- as.numeric(smooth(sigmaTheta[ok]))
            sigmaThetaDeriv <- c(0, diff(sigmaThetaSmooth) / diff(pressure))
        }
    } else {
        sigmaThetaDeriv <- derivs(pressure, sigmaTheta)
    }
    ifelse(ok, 9.8 * 9.8 * 1e-4 * sigmaThetaDeriv, NA)
}

swSCTp <- function(conductivity, temperature, pressure)
{
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

swSTrho <- function(temperature, density, pressure) # FIXME: should be vectorized for speed
{
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
                     as.double(temperature[i]),
                     as.double(sigma),
                     as.double(pressure[i]),
                     S = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$S
    	if (i == 1) rval <- this.S else rval <- c(rval, this.S)
    }
    dim(rval) <- dim
    rval
}

swTSrho <- function(salinity, density, pressure) # FIXME: should be vectorized
{
    if (missing(salinity))
        stop("must provide salinity")
    dim <- dim(salinity)
    nS <- length(salinity)
    nrho <- length(density)
    if (is.null(pressure))
        pressure <- rep(0, nS)
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
    	this.T <- .C("sw_tsrho",
                     as.double(salinity[i]),
                     as.double(sig),
                     as.double(pressure[i]),
                     temperature = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$t
    	if (i == 1) rval <- this.T else rval <- c(rval, this.T)
    }
    dim(rval) <- dim
    rval
}

swTFreeze <- function(salinity, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    (-.0575+1.710523e-3*sqrt(abs(salinity))-2.154996e-4*salinity)*salinity-7.53e-4*pressure
}

swAlpha <- function(salinity, temperature=NULL, pressure=NULL, isTheta = FALSE)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    nS <- length(salinity)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    swAlphaOverBeta(salinity, temperature, pressure, isTheta) * swBeta(salinity, temperature, pressure, isTheta)
}

swAlphaOverBeta <- function(salinity, temperature=NULL, pressure=NULL, isTheta = FALSE)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(pressure[1], np)
    }
    if (!isTheta)
        t = swTheta(salinity, temperature, pressure)
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_alpha_over_beta",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

swBeta <- function(salinity, temperature=NULL, pressure=NULL, isTheta = FALSE)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (!isTheta)
        temperature = swTheta(salinity, temperature, pressure)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        pressure <- rep(pressure[1], np)
    }
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_beta",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

swConductivity <- function (salinity, temperature=NULL, pressure=NULL)
{
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    return(0.57057 * (1 + temperature * (0.003 - 1.025e-05 * temperature) + 0.000653 * pressure - 0.00029 * salinity))
}

swDepth <- function(pressure, latitude=45, degrees=TRUE)
{
    if (inherits(pressure, "ctd")) {
        latitude <- abs(pressure$metadata$latitude)
        pressure <- pressure$data$pressure # over-writes pressure
    }
    if (degrees) latitude <- latitude * 0.0174532925199433
    x <- sin(latitude)^2
    gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*pressure
    (((-1.82e-15*pressure+2.279e-10)*pressure-2.2512e-5)*pressure+9.72659)*pressure / gr
}

swZ <- function(pressure, latitude=45, degrees=TRUE)
{
    -swDepth(pressure=pressure, latitude=latitude, degrees=degrees)
}

swDynamicHeight <- function(x, referencePressure=2000)
{
    height <- function(ctd, referencePressure)
    {
        if (sum(!is.na(ctd@data$pressure)) < 2) return(NA) # cannot integrate then
        g <- if (is.na(ctd@metadata$latitude)) 9.8 else gravity(ctd@metadata$latitude)
        np <- length(ctd@data$pressure)
        rho <- swRho(ctd)
        if (sum(!is.na(rho)) < 2) return(NA)
        ## 1e4 converts decibar to Pa
        dzdp <- ((1/rho - 1/swRho(rep(35,np),rep(0,np),ctd@data$pressure))/g)*1e4
##        print(summary(ctd))
        integrand <- approxfun(ctd@data$pressure, dzdp, rule=2)
        integrate(integrand, 0, referencePressure)$value
    }
    if (inherits(x, "section")) {
        lon0 <- x@data$station[[1]]@metadata$longitude
        lat0 <- x@data$station[[1]]@metadata$latitude
        ns <- length(x@data$station)
        d <- vector("numeric", ns)
        h <- vector("numeric", ns)
        for (i in 1:ns) {               # FIXME: avoid loops
##            cat("i=",i,"\n")
            d[i] <- geodDist(x@data$station[[i]]@metadata$latitude, x@data$station[[i]]@metadata$longitude, lat0, lon0)
            h[i] <- height(x@data$station[[i]], referencePressure)
        }
        return(list(distance=d, height=h))
    } else if (inherits(x, "ctd")) {
        return(height(x, referencePressure))
    } else {
        stop("method only works for 'section' or 'ctd' objects")
    }
}

swLapseRate <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_lapserate",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

swRho <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity)) 
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    ## sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(pressure[1], np)
    }
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_rho",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

swSigma <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    swRho(salinity, temperature, pressure) - 1000
}

swSigmaT <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    ptop <- rep(0, length(salinity))
    swRho(salinity, temperature, ptop) - 1000
}

swSigmaTheta <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    ptop <- rep(0, length(salinity))
    swRho(salinity, swTheta(salinity, temperature, pressure), ptop) - 1000
}

swSoundSpeed <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(pressure[1], np)
    }
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_svel",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

## Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
## check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
swSpecificHeat <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <-  salinity@data$temperature
        pressure <-  salinity@data$pressure
        salinity <-  salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    for (i in 1:nS) {                   # FIXME: avoid loops
    	this.CP <- .Fortran("ocecp", as.double(salinity[i]),
                            as.double(temperature[i]), as.double(pressure[i]),
                            CP = double(1), PACKAGE = "oce")$CP
    	if (i == 1)
            rval <- this.CP
        else
            rval <- c(rval, this.CP)
    }
    dim(rval) <- dim
    rval
}

swSpice <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    ## sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(pressure[1], np)
    }
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_spice",
               as.integer(nS),
               as.double(salinity),
               as.double(temperature),
               as.double(pressure),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

swTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0, method=c("unesco", "bryden"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            tmp <- makeS4(salinity)
        else
            tmp <- salinity
        salinity <- tmp@data$salinity
        temperature <- tmp@data$temperature
        pressure <- tmp@data$pressure
    }
    if (is.null(temperature))
        stop("must provide temperature")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(pressure))
        pressure <- rep(0, nS)
    np <- length(pressure)
    if (np == 1) {
        np <- nS
        p <- rep(pressure[1], np)
    }
    method <- match.arg(method)
    if (method == "bryden") {
        rval <- .C("theta_Bryden_1973",
                   as.integer(nS), as.double(salinity), as.double(temperature), as.double(pressure),
                   value = double(nS),
                   NAOK=TRUE,
                   PACKAGE = "oce")$value
    } else {
        if (method == "unesco") {
                                        # sometimes have just a single value
            npref <- length(referencePressure)
            if (npref == 1)
                referencePressure <- rep(referencePressure[1], nS)
            rval <- .C("theta_UNESCO_1983",
                       as.integer(nS), as.double(salinity), as.double(temperature), as.double(pressure), as.double(referencePressure),
                       value = double(nS),
                       NAOK=TRUE, PACKAGE = "oce")$value
        } else {
            stop("unrecognized method=\"", method, "\"")
        }
    }
    dim(rval) <- dim
    rval
}

swViscosity <- function(salinity, temperature=NULL)
{
    if (inherits(salinity, "ctd")) {
        if (!isS4(salinity))
            salinity <- makeS4(salinity)
        temperature <-  salinity@data$temperature
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    0.001798525 + salinity * (2.634749e-06 - 7.088328e-10 *
           temperature^2 + salinity * (-4.702342e-09 + salinity *
           (5.32178e-11))) + temperature * (-6.293088e-05 +
           temperature * (1.716685e-06 + temperature * (-3.479273e-08
           + temperature * (+3.566255e-10))))
}
