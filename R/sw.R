sw.N2 <- function(p, sigma.theta=NULL, ...) # BUG: think more about best density measure
{
    if (inherits(p, "ctd")) {
        sigma.theta <- sw.sigma.theta(p$data$salinity, p$data$temperature, p$data$pressure)
        p <- p$data$pressure # over-writes p
    }
    args <- list(...)
    ## df <- if (is.null(args$df)) length(p)/5 else args$df;
    df <- if (is.null(args$df)) min(length(p)/5, 10) else args$df;
    ok <- !is.na(p) & !is.na(sigma.theta)
    sigma.theta.smooth <- smooth.spline(p[ok], sigma.theta[ok], df=df)
    sigma.theta.deriv <- rep(NA, length(p))
    sigma.theta.deriv[ok] <- predict(sigma.theta.smooth, p[ok], deriv = 1)$y
    ifelse(ok, 9.8 * 9.8 * 1e-4 * sigma.theta.deriv, NA)
}

sw.S.C.T.p <- function(C, t, p)
{
    dim <- dim(C)
    nC <- length(C)
    nt <- length(t)
    np <- length(p)
    if (nC != nt) stop("lengths of C and t must agree, but they are ", nC, " and ", nt, ", respectively")
    if (nC != np) stop("lengths of C and p must agree, but they are ", nC, " and ", np, ", respectively")
    rval <- .C("sw_salinity",
               as.integer(nC),
               as.double(C),
               as.double(t),
               as.double(p),
               value = double(nC),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.S.T.rho <- function(t, rho, p) # FIXME: should be vectorized for speed
{
    dim <- dim(t)
    nt <- length(t)
    nrho <- length(rho)
    np <- length(p)
    if (nt != nt) stop("lengths of temperature and density must agree, but they are ", nt, " and ", nrho, ", respectively")
    if (nt != np) stop("lengths of temperature and p arrays must agree, but they are ", nt, " and ", np, ", respectively")
    for (i in 1:nt) {
    	sig <- rho[i]
    	if (sig > 500) sig <- sig - 1000
    	this.S <- .C("sw_strho",
                     as.double(t[i]),
                     as.double(sig),
                     as.double(p[i]),
                     S = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$S
    	if (i == 1) rval <- this.S else rval <- c(rval, this.S)
    }
    dim(rval) <- dim
    rval
}

sw.T.S.rho <- function(S, rho, p) # FIXME: should be vectorized
{
    if (missing(S)) stop("must provide S")
    dim <- dim(S)
    nS <- length(S)
    nrho <- length(rho)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nrho) stop("lengths of S and rho must agree, but they are ", nS, " and ", nrho,  ", respectively")
    if (nS != np)   stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    for (i in 1:nS) {
    	sig <- rho[i]
    	if (sig > 500) {
            sig <- sig - 1000
    	}
    	this.t <- .C("sw_tsrho",
                     as.double(S[i]),
                     as.double(sig),
                     as.double(p[i]),
                     t = double(1),
                     NAOK=TRUE, PACKAGE = "oce")$t
    	if (i == 1) rval <- this.t else rval <- c(rval, this.t)
    }
    dim(rval) <- dim
    rval
}

sw.T.freeze <- function(S, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    (-.0575+1.710523e-3*sqrt(abs(S))-2.154996e-4*S)*S-7.53e-4*p
}

sw.alpha <- function(S, t=NULL, p=NULL, is.theta = FALSE)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    nS <- length(S)
    if (is.null(p)) p <- rep(0, nS)
    sw.alpha.over.beta(S, t, p, is.theta) * sw.beta(S, t, p, is.theta)
}

sw.alpha.over.beta <- function(S, t=NULL, p=NULL, is.theta = FALSE)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    if (!is.theta)
        t = sw.theta(S, t, p)
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_alpha_over_beta",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.beta <- function(S, t=NULL, p=NULL, is.theta = FALSE)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (!is.theta) t = sw.theta(S, t, p)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_beta",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.conductivity <- function (S, t=NULL, p=NULL)
{
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    return(0.57057 * (1 + t * (0.003 - 1.025e-05 * t) + 0.000653 * p - 0.00029 * S))
}

sw.depth <- function(p, lat=45, degrees=TRUE)
{
    if (inherits(p, "ctd")) {
        lat <- abs(p$metadata$latitude)
        p <- p$data$pressure # over-writes p
    }
    if (degrees) lat <- lat * 0.0174532925199433
    x <- sin(lat)^2
    gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*p
    (((-1.82e-15*p+2.279e-10)*p-2.2512e-5)*p+9.72659)*p / gr
}

sw.z <- function(p, lat=45, degrees=TRUE)
{
    -sw.depth(p=p, lat=lat, degrees=degrees)
}

sw.dynamic.height <- function(x, pref=2000)
{
    height <- function(ctd, pref)
    {
        if (sum(!is.na(ctd$data$pressure)) < 2) return(NA) # cannot integrate then
        g <- if (is.na(ctd$metadata$latitude)) 9.8 else gravity(ctd$metadata$latitude)
        np <- length(ctd$data$pressure)
        rho <- sw.rho(ctd)
        if (sum(!is.na(rho)) < 2) return(NA)
        ## 1e4 converts decibar to Pa
        dzdp <- ((1/rho - 1/sw.rho(rep(35,np),rep(0,np),ctd$data$pressure))/g)*1e4
##        print(summary(ctd))
        integrand <- approxfun(ctd$data$pressure, dzdp, rule=2)
        integrate(integrand, 0, pref)$value
    }
    if (inherits(x, "section")) {
        lon0 <- x$data$station[[1]]$metadata$longitude
        lat0 <- x$data$station[[1]]$metadata$latitude
        ns <- length(x$data$station)
        d <- vector("numeric", ns)
        h <- vector("numeric", ns)
        for (i in 1:ns) {
##            cat("i=",i,"\n")
            d[i] <- geod.dist(x$data$station[[i]]$metadata$latitude, x$data$station[[i]]$metadata$longitude, lat0, lon0)
            h[i] <- height(x$data$station[[i]], pref)
        }
        return(list(distance=d, height=h))
    } else if (inherits(x, "ctd")) {
        return(height(x, pref))
    } else {
        stop("method only works for 'section' or 'ctd' objects")
    }
}

sw.lapse.rate <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_lapserate",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.rho <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
    ## sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_rho",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.sigma <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    sw.rho(S, t, p) - 1000
}

sw.sigma.t <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    p.top <- rep(0, length(S))
    sw.rho(S, t, p.top) - 1000
}

sw.sigma.theta <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    p.top <- rep(0, length(S))
    sw.rho(S, sw.theta(S, t, p), p.top) - 1000
}

sw.sound.speed <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
                                        # sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_svel",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

## Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
## check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
sw.specific.heat <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    for (i in 1:nS) {
    	this.CP <- .Fortran("ocecp", as.double(S[i]),
                            as.double(t[i]), as.double(p[i]),
                            CP = double(1), PACKAGE = "oce")$CP
    	if (i == 1)
            rval <- this.CP
        else
            rval <- c(rval, this.CP)
    }
    dim(rval) <- dim
    rval
}

sw.spice <- function(S, t=NULL, p=NULL)
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
    ## sometimes give just a single p value (e.g. for a TS diagram)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
    rval <- .C("sw_spice",
               as.integer(nS),
               as.double(S),
               as.double(t),
               as.double(p),
               value = double(nS),
               NAOK=TRUE, PACKAGE = "oce")$value
    dim(rval) <- dim
    rval
}

sw.theta <- function(S, t=NULL, p=NULL, pref=0, method=c("UNESCO1983", "Bryden1973"))
{
    if (missing(S)) stop("must provide S")
    if (inherits(S, "ctd")) {
        tmp <- S
        S <- tmp$data$salinity
        t <- tmp$data$temperature
        p <- tmp$data$pressure
    }
    if (is.null(t)) stop("must provide temperature")
    dim <- dim(S)
    nS <- length(S)
    nt <- length(t)
    if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(p)) p <- rep(0, nS)
    np <- length(p)
    if (np == 1) {
        np <- nS
        p <- rep(p[1], np)
    }
    method <- match.arg(method)
    if (method == "Bryden1973") {
        rval <- .C("theta_Bryden_1973",
                   as.integer(nS), as.double(S), as.double(t), as.double(p),
                   value = double(nS),
                   NAOK=TRUE,
                   PACKAGE = "oce")$value
    } else {
        if (method == "UNESCO1983") {
                                        # sometimes have just a single value
            npref <- length(pref)
            if (npref == 1)
                pref <- rep(pref[1], nS)
            rval <- .C("theta_UNESCO_1983",
                       as.integer(nS), as.double(S), as.double(t), as.double(p), as.double(pref),
                       value = double(nS),
                       NAOK=TRUE, PACKAGE = "oce")$value
        } else {
            stop("unrecognized method=\"", method, "\"")
        }
    }
    dim(rval) <- dim
    rval
}
sw.viscosity <- function (S, t=NULL)
{
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        S <- S$data$salinity # note: this destroys the ctd object
    }
    return(0.001798525 + S * (2.634749e-06 - 7.088328e-10 * t^2 +
                              S * (-4.702342e-09 + S * (5.32178e-11))) + t * (-6.293088e-05 +
                                                                              t * (1.716685e-06 + t * (-3.479273e-08 + t * (+3.566255e-10)))))
}
