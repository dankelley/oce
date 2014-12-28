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

## FIXME: should be vectorized for speed
## FIXME-gsw: should use gsw
swSTrho <- function(temperature, density, pressure, eos=getOption("oceEOS", default="gsw"))
{
    eos <- match.arg(eos, c("unesco", "gsw", "teos"))
    teos <- eos == "teos" || eos == "gsw"
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
swTSrho <- function(salinity, density, pressure, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    eos <- match.arg(eos, c("unesco", "gsw", "teos"))
    teos <- eos == "teos" || eos == "gsw"
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

swTFreeze <- function(salinity, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
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
        pressure <- rep(pressure[1], np)
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
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    return(0.57057 * (1 + temperature * (0.003 - 1.025e-05 * temperature) + 0.000653 * pressure - 0.00029 * salinity))
}

swDepth <- function(pressure, latitude=45, degrees=TRUE)
{
    if (inherits(pressure, "ctd")) {
        latitude <- abs(pressure@metadata$latitude)
        pressure <- pressure@data$pressure # over-writes pressure
    }
    if (degrees) latitude <- latitude * 0.0174532925199433
    x <- sin(latitude)^2
    gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*pressure
    (((-1.82e-15*pressure+2.279e-10)*pressure-2.2512e-5)*pressure+9.72659)*pressure / gr
}

swZ <- function(pressure, latitude=45, degrees=TRUE)
{
    if (inherits(pressure, "ctd")) {
        keep <- pressure
        pressure <- keep[["pressure"]]
        latitude <- keep[["latitude"]]
    }
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

swLapseRate <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
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

swRho <- function(salinity, temperature=NULL, pressure=NULL, longitude, latitude,
                  eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    eos <- match.arg(eos, c("unesco", "gsw", "teos"))
    if (inherits(salinity, "ctd")) {
        ctd <- salinity
        salinity <- ctd[["salinity"]]
        n <- length(salinity)
        temperature <- ctd[["temperature"]]
        pressure <- ctd[["pressure"]]
        if (missing(longitude))
            longitude <- rep(ctd[["longitude"]], length.out=n)
        if (missing(latitude))
            latitude <- rep(ctd[["latitude"]], length.out=n)
    } else if (inherits(salinity, "section")) {
        section <- salinity
        salinity <- section[["salinity"]]
        n <- length(salinity)
        temperature <- section[["temperature"]]
        pressure <- section[["pressure"]]
        if (missing(longitude))
            longitude <- rep(section[["longitude"]], length.out=n)
        if (missing(latitude))
            latitude <- rep(section[["latitude"]], length.out=n)
    }
    ## FIXME handle the 'section' case similarly to the 'ctd' case.
    if (is.null(temperature))
        stop("must provide temperature")
    if (is.null(pressure))
        stop("must provide pressure")
    dim <- dim(salinity) # can be used later to reshape
    nt <- length(temperature)
    if (nt != n)
        stop("mismatched lengths of salinity and temperature: ", n, " and ", nt)
    if (is.null(pressure))
        pressure <- rep(0, n)
    np <- length(pressure)
    ## sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1 && n > np) {
        np <- n
        pressure <- rep(pressure[1], np)
    }
    if (n != np)
        stop("lengths of salinity and pressure must agree, but they are ", n, " and ", np, ", respectively")
    if (eos == "unesco") {
        rval <- .C("sw_rho",
                   as.integer(n),
                   as.double(salinity),
                   as.double(temperature),
                   as.double(pressure),
                   value = double(n),
                   NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw" || eos == "teos") {
        if (missing(latitude))
            latitude <- rep(30, np) # arbitrary spot in mid atlantic
        if (missing(longitude))
            longitude <- rep(320, np)
        SA <- gsw_SA_from_SP(salinity, pressure, longitude, latitude)
        CT <- gsw_CT_from_t(SA, temperature, pressure)
        rval <- gsw_rho(SA, CT, pressure) # FIXME-gsw maybe should use exact form
    } else {
        stop("unknown eos: ", eos) # cannot reach here, given match.arg() above
    }
    if (is.matrix(salinity))
        dim(rval) <- dim
    rval
}

swSigma <- function(salinity, temperature=NULL, pressure=NULL, longitude, latitude,
                    eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    eos <- match.arg(eos, c("unesco", "gsw", "teos"))
    if (inherits(salinity, "ctd")) {
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    swRho(salinity, temperature, pressure, eos=eos, longitude=longitude, latitude=latitude) - 1000
}

swSigmaT <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    ptop <- rep(0, length(salinity))
    swRho(salinity, temperature, ptop) - 1000
}

## FIXME-gsw: should permit different EOS (so needs also lon and lat)
swSigmaTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
        names <- names(salinity@data)
        if (!("temperature" %in% names)) stop("CTD object lacks temperature data")
        if (!("salinity" %in% names)) stop("CTD object lacks salinity data")
        if (!("pressure" %in% names)) stop("CTD object lacks pressure data")
        ## FIXME: should ensure that these 
        temperature <- salinity@data$temperature
        pressure <- salinity@data$pressure
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    if (is.null(temperature)) stop("must supply temperature")
    if (is.null(pressure)) stop("must supply pressure")
    referencePressure <- rep(referencePressure, length.out=length(salinity))
    swRho(salinity, swTheta(salinity, temperature, pressure), referencePressure) - 1000
}

swSigma0 <- function(salinity, temperature, pressure)
    swSigmaTheta(salinity, temperature, pressure, referencePressure=0)
swSigma1 <- function(salinity, temperature, pressure)
    swSigmaTheta(salinity, temperature, pressure, referencePressure=1000)
swSigma2 <- function(salinity, temperature, pressure)
    swSigmaTheta(salinity, temperature, pressure, referencePressure=2000)
swSigma3 <- function(salinity, temperature, pressure)
    swSigmaTheta(salinity, temperature, pressure, referencePressure=3000)
swSigma4 <- function(salinity, temperature, pressure)
    swSigmaTheta(salinity, temperature, pressure, referencePressure=4000)

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

swSoundSpeed <- function(salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
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
        pressure <- rep(pressure[1], np)
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
        pressure <- rep(pressure[1], np)
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

swTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
                    method=c("unesco", "bryden", "gsw", "teos"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "ctd")) {
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
        pressure <- rep(pressure[1], np)
    }
    method <- match.arg(method)
    if (method == "bryden") {
        rval <- .C("theta_Bryden_1973",
                   as.integer(nS), as.double(salinity), as.double(temperature), as.double(pressure),
                   value = double(nS),
                   NAOK=TRUE,
                   PACKAGE = "oce")$value
    } else if (method == "gsw" || method == "teos") {
        SA <- gsw_SA_from_SP(salinity, pressure, 188, 4)
        rval <- gsw_pt0_from_t(SA, temperature, pressure) # FIXME-gsw: should have args for lon and lat
    } else if (method == "unesco") {
            ## sometimes have just a single value
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
    dim(rval) <- dim
    rval
}

swViscosity <- function(salinity, temperature=NULL)
{
    if (inherits(salinity, "ctd")) {
        temperature <-  salinity@data$temperature
        salinity <- salinity@data$salinity # note: this destroys the ctd object
    }
    0.001798525 + salinity * (2.634749e-06 - 7.088328e-10 *
           temperature^2 + salinity * (-4.702342e-09 + salinity *
           (5.32178e-11))) + temperature * (-6.293088e-05 +
           temperature * (1.716685e-06 + temperature * (-3.479273e-08
           + temperature * (+3.566255e-10))))
}

swConservativeTemperature <- function(salinity, temperature, pressure)
{
    if (inherits(salinity, "ctd")) {
        temperature <-  salinity@data$temperature
        pressure <-  salinity@data$pressure
        salinity <- salinity@data$salinity # NOTE: this destroys the salinity object
    } else {
        if (missing(temperature)) stop("must provide temperature")
        if (missing(pressure)) stop("must provide temperature")
    }
    n <- length(salinity)
    if (n != length(temperature)) stop("lengths of salinity and temperature must match") 
    if (n != length(pressure)) stop("lengths of salinity and pressure must match") 
    bad <- is.na(salinity) | is.na(temperature) | is.na(pressure)
    good <- gsw_CT_from_t(salinity[!bad], temperature[!bad], pressure[!bad])
    rval <- rep(NA, n)
    rval[!bad] <- good
    rval
}

swAbsoluteSalinity <- function(salinity, pressure, longitude, latitude)
{
    if (inherits(salinity, "ctd")) {
        pressure <- salinity@data$pressure
        n <- length(pressure)
        longitude <- rep(salinity@metadata$longitude, n)
        latitude <- rep(salinity@metadata$latitude, n)
        salinity <- salinity@data$salinity # NOTE: this destroys the salinity object
    } else {
        ## FIXME: perhaps should default lon and lat
        n <- length(salinity)
        if (missing(pressure)) stop("must provide temperature")
        if (missing(longitude)) stop("must provide longitude to compute absolute salinity")
        if (missing(latitude)) stop("must provide latitude to compute absolute salinity")
    }
    if (n != length(pressure)) stop("lengths of salinity and pressure must match") 
    if (n != length(longitude)) stop("lengths of salinity and longitude must match")  
    if (n != length(latitude))  stop("lengths of salinity and latitude must match") 
    longitude <- ifelse(longitude < 0, longitude + 360, longitude)
    bad <- is.na(salinity) | is.na(pressure) | is.na(longitude) | is.na(latitude)
    good <- gsw_SA_from_SP(salinity[!bad], pressure[!bad], longitude[!bad], latitude[!bad])
    rval <- rep(NA, n)
    rval[!bad] <- good
    rval
}
