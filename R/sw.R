#' @title Convert from ITS-90 to IPTS-68 temperature
#' 
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the ITS-90 scale.
#' @return Temperature expressed in the IPTS-68 scale.
T68fromT90 <- function(temperature) temperature * 1.00024

#' @title Convert from IPTS-68 to ITS-90 temperature
#' 
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the IPTS-68 scale.
#' @return temperature Temperature expressed in the ITS-90 scale.
T90fromT68 <- function(temperature) temperature / 1.00024

#' @title Convert from ITS-48 to ITS-90 temperature
#' 
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the ITS-48 scale.
#' @return Temperature expressed in the ITS-90 scale.
T90fromT48 <- function(temperature) (temperature-4.4e-6*temperature*(100-temperature))/1.00024

#' Look Within the First Element of a List for Replacement Values
#'
#' @details
#' This is a helper function used by various seawater functions. It is used for a
#' call like \code{\link{swRho}(ctd)}, in which the first argument, which is
#' normally \code{salinity} may be an object that contains salinity plus
#' the other items that \code{\link{swRho}} expects to see as arguments. This
#' shorthand is very helpful in calls to the suite of \code{sw} functions.  If
#' this first argument is an object of this sort, then the other arguments
#' are ignored \emph{except} for two special cases:
#' \itemize{
#' \item an item named \code{eos} is copied directly from \code{list}
#' \item if the object stores \code{temperature} defined with the IPTS-68
#' scale, then \code{\link{T90fromT68}} is used to convert to the ITS-90 scale,
#' because this is what is expected in most seawater functions. (For example,
#' the RMS difference between these temperature variants is 0.002C for the 
#' \code{\link{ctd}} dataset.)
#' }
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
            ##message("names[", i, "]: ", names[i])
            if ("eos" != names[i]) {
                ## Note: the accessor [[]] will return temperature
                ## in ITS-90 regardless of how it is stored, and similarly pressure
                ## in dbar and salinity in FIXME: what unit to use??
                try({
                    list[[i]] <- list1[[names[i], "nowarn"]]
                }, silent=TRUE)
            }
        }
        if (inherits(list1, "ctd")) {
            nrows <- length(list[[names[1]]])
            list[["longitude"]] <- rep(list[["longitude"]][1], nrows)
            list[["latitude"]] <- rep(list[["latitude"]][1], nrows)
        }
        ## FIXME: should special-case some other object types
    }
    if ("eos" %in% names)
        list[["eos"]] <- match.arg(list[["eos"]], c("unesco", "gsw"))
    list
}


#' Density ratio
#'
#' Compute density ratio
#'
#' This computes Rrho (density ratio) from a \code{ctd} object.
#'
#' If \code{eos="unesco"}, this is done by calculating salinity and
#' potential-temperature derivatives from smoothing splines whose properties
#' are governed by \code{smoothingLength} or \code{df}.  If
#' \code{sense="diffusive"} the definition is
#' \eqn{(beta*dS/dz)/(alpha*d(theta)/dz)}{(beta*dS/dz)/(alpha*d(theta)/dz)} and
#' the reciprocal for \code{"finger"}.
#'
#' If \code{eos="gsw"}, this is done by extracting absolute salinity and
#' conservative temperature, smoothing with a smoothing spline as in the
#' \code{"unesco"} case, and then calling \code{\link[gsw]{gsw_Turner_Rsubrho}}
#' on these smoothed fields. Since the gsw function works on mid-point
#' pressures, \code{\link{approx}} is used to interpolate back to the original
#' pressures.
#'
#' If the default arguments are acceptable, \code{ctd[["Rrho"]]} may be used
#' instead of \code{swRrho(ctd)}.
#'
#' @param ctd an object of class \code{ctd}
#' @param sense an indication of the sense of double diffusion under study and
#' therefore of the definition of Rrho; see \sQuote{Details}
#' @param smoothingLength ignored if \code{df} supplied, but otherwise the
#' latter is calculated as the number of data points, divided by the number
#' within a depth interval of \code{smoothingLength} metres.
#' @param df if given, this is provided to \code{\link{smooth.spline}}.
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Density ratio defined in either the \code{"diffusive"} or
#' \code{"finger"} sense.
#' @author Dan Kelley and Chantelle Layton
#' @examples
#' library(oce)
#' data(ctd)
#' u <- swRrho(ctd, eos="unesco")
#' g <- swRrho(ctd, eos="gsw")
#' p <- ctd[["p"]]
#' plot(u, p, ylim=rev(range(p)), type='l', xlab=expression(R[rho]))
#' lines(g, p, lty=2, col='red')
#' legend("topright", lty=1:2, legend=c("unesco", "gsw"), col=c("black", "red"))
#' 
#' @family functions that calculate seawater properties
swRrho <- function(ctd, sense=c("diffusive", "finger"), smoothingLength=10, df,
                   eos=getOption("oceEOS", default="gsw"))
{
    if (!inherits(ctd, "ctd"))
        stop("first argument must be of class \"ctd\"")
    sense <- match.arg(sense)
    eos <- match.arg(eos, c("unesco", "gsw"))
    p <- ctd[['pressure']]
    n <- length(p)
    if (n < 4)
        return(rep(NA, length.out=n))
    A <- smoothingLength / mean(diff(p), na.rm=TRUE)
    if (missing(df))
        df <- n / A
    if (eos == "unesco") {
        salinity <- ctd[['salinity']]
        temperature <- ctd[['temperature']]
        theta <- ctd[['theta']]
        ok <- !is.na(p) & !is.na(salinity) & !is.na(temperature)
        ## infer d(theta)/dp and d(salinity)/dp from smoothing splines
        temperatureSpline <- smooth.spline(p[ok], temperature[ok], df=df)
        salinitySpline <- smooth.spline(p[ok], salinity[ok], df=df)
        ## Smooth temperature and salinity to get smoothed alpha and beta
        CTD <- as.ctd(predict(salinitySpline, p)$y, predict(temperatureSpline, p)$y, p)
        alpha <- swAlpha(CTD, eos="unesco")
        beta <- swBeta(CTD, eos="unesco")
        ## Using alpha ... is that right, since we have theta?
        thetaSpline <- smooth.spline(p[ok], theta[ok], df=df)
        dthetadp <- predict(thetaSpline, p, deriv=1)$y
        dsalinitydp <- predict(salinitySpline, p, deriv=1)$y
        Rrho <- if (sense == "diffusive") (beta * dsalinitydp)/ (alpha * dthetadp) else
            (alpha * dthetadp) / (beta * dsalinitydp)
    } else if (eos == "gsw") {
        SA <- ctd[["SA"]]
        CT <- ctd[["CT"]]
        ok <- !is.na(p) & !is.na(SA) & !is.na(CT)
        SA <- predict(smooth.spline(p[ok], SA[ok], df=df), p)$y
        CT <- predict(smooth.spline(p[ok], CT[ok], df=df), p)$y
        a <- gsw::gsw_Turner_Rsubrho(SA, CT, p)
        Rrho <- a$Rsubrho
        Rrho[Rrho==9e15] <- NA
        Rrho <- approx(a$p_mid, Rrho, p, rule=2)$y
        if (sense == "diffusive")
            Rrho <- 1 / Rrho
    }
    Rrho
}


#' Squared buoyancy frequency for seawater
#'
#' Compute \eqn{N^2}{N^2}, the square of the buoyancy frequency for a seawater
#' profile.
#'
#' Smoothing is often useful prior to computing buoyancy frequency, and so this
#' may optionally be done with \code{\link{smooth.spline}}, unless
#' \code{df=NA}, in which case raw data are used.  If \code{df} is not
#' provided, a possibly reasonable value computed from an analysis of the
#' profile, based on the number of pressure levels.
#'
#' If \code{eos="gsw"}, then the first argument must be a \code{ctd} object,
#' and processing is done with \code{\link[gsw]{gsw_Nsquared}}, based on
#' extracted values of Absolute Salinity and Conservative Temperature (possibly
#' smoothed, depending on \code{df}).
#'
#' If \code{eos="unesco"}, then the processing is as follows.  The core of the
#' method involves differentiating potential density (referenced to median
#' pressure) with respect to pressure, and the \code{derivs} argument is used
#' to control how this is done, as follows.
#'
#' \itemize{
#'
#' \item if \code{derivs} is not supplied, the action is as though it were
#' given as the string \code{"smoothing"}
#'
#' \item if \code{derivs} equals \code{"simple"}, then the derivative of
#' density with respect to pressure is calculated as the ratio of first-order
#' derivatives of density and pressure, each calculated using
#' \code{\link{diff}}.  (A zero is appended at the top level.)
#'
#' \item if \code{derivs} equals \code{"smoothing"}, then the processing
#' depends on the number of data in the profile, and on whether \code{df} is
#' given as an optional argument.  When the number of points exceeds 4, and
#' when \code{df} exceeds 1, \code{\link{smooth.spline}} is used to calculate
#' smoothing spline representation the variation of density as a function of
#' pressure, and derivatives are extracted from the spline using
#' \code{predict}.  Otherwise, density is smoothed using \code{\link{smooth}},
#' and derivatives are calculated as with the \code{"simple"} method.
#'
#' \item if \code{derivs} is a function taking two arguments (first pressure,
#' then density) then that function is called directly to calculate the
#' derivative, and no smoothing is done before or after that call.  }
#'
#' For deep-sea work, the \code{eos="gsw"} option is the best scheme, because
#' it uses derivatives of density computed with \emph{local} reference
#' pressure.
#'
#' For precise work, it makes sense to skip \code{swN2} entirely, choosing
#' whether, what, and how to smooth based on an understanding of fundamental
#' principles as well as data practicalities.
#'
#' @param pressure either pressure [dbar] (in which case \code{sigmaTheta} must
#' be provided) \strong{or} an object of class \code{ctd} object (in which case
#' \code{sigmaTheta} is inferred from the object.
#' @param sigmaTheta Surface-referenced potential density minus 1000
#' [kg/m\eqn{^3}{^3}]
#' @param derivs optional argument to control how the derivative
#' \eqn{d\sigma_\theta/dp}{d(sigmaTheta)/d(pressure)} is calculated.  This may
#' be a character string or a function of two arguments.  See \dQuote{Details}.
#' @param df argument passed to \code{\link{smooth.spline}} if this function is
#' used for smoothing; set to \code{NA} to prevent smoothing.
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @param \dots additional argument, passed to \code{\link{smooth.spline}}, in
#' the case that \code{derivs="smoothing"}.  See \dQuote{Details}.
#' @return Square of buoyancy frequency [\eqn{radian^2/s^2}{radian^2/s^2}].
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(ctd)
#' # Illustrate difference between UNESCO and GSW
#' p <- ctd[["pressure"]]
#' ylim <- rev(range(p))
#' par(mfrow=c(1,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(ctd[["sigmaTheta"]], p, ylim=ylim, type='l', xlab=expression(sigma[theta]))
#' N2u <- swN2(ctd, eos="unesco")
#' N2g <- swN2(ctd, eos="gsw")
#' plot(N2u, p, ylim=ylim, xlab="N2 Unesco", ylab="p", type="l")
#' d <- 100 * (N2u - N2g) / N2g
#' plot(d, p, ylim=ylim, xlab="N2 UNESCO-GSW diff. [%]", ylab="p", type="l")
#' abline(v=0)
#' @family functions that calculate seawater properties
swN2 <- function(pressure, sigmaTheta=NULL, derivs, df,
                 eos=getOption("oceEOS", default="gsw"), ...)
{
    ##cat("swN2(..., df=", df, ")\n",sep="")
    eos <- match.arg(eos, c("unesco", "gsw"))
    ##useSmoothing <- !missing(df) && is.finite(df)
    if (eos == "unesco") {
        if (inherits(pressure, "ctd")) {
            pref <- median(pressure[["pressure"]], na.rm=TRUE)
            sigmaTheta <- swSigmaTheta(pressure, referencePressure=pref)
            pressure <- pressure[['pressure']] # over-writes pressure
        }
        if (missing(derivs))
            derivs <- "smoothing"
        ok <- !is.na(pressure) & !is.na(sigmaTheta)
        if (is.character(derivs)) {
            if (derivs == "simple") {
                sigmaThetaDeriv <- c(0, diff(sigmaTheta) / diff(pressure))
            } else if (derivs == "smoothing") {
                depths <- sum(!is.na(pressure))
                if (missing(df)) {
                    df <- if (depths > 100) f <- floor(depths / 10) # at least 10
                        else if (depths > 20) f <- floor(depths / 3) # at least 7
                        else if (depths > 10) f <- floor(depths / 2) # at least 5
                        else depths
                    oceDebug(getOption("oceDebug"), "df not supplied, so set to ", df, "(note: #depths=", depths, ")\n")
                }
                if (depths > 4 && df > 5) {
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
        ## FIXME (DK 2016-05-04) I am not sure I like the following since it
        ## uses a standardized rho_0. But it's from some official source I think.
        ## Must check this. (UNESCO book?)
        res <- ifelse(ok, 9.8 * 9.8 * 1e-4 * sigmaThetaDeriv, NA)
    } else if (eos == "gsw") {
        if (!inherits(pressure, "ctd"))
            stop("first argument must be a CTD object if eos=\"gsw\"")
        ctd <- pressure
        SA <- ctd[["SA"]]
        CT <- ctd[["CT"]]
        p <- ctd[["p"]]
        ##np <- length(p)
        ok <- !is.na(p) & !is.na(SA) & !is.na(CT)
        if (missing(df))
            df <- round(length(p[ok]) / 10)
        if (length(p[ok]) > 4 && is.finite(df)) {
            SA <- predict(smooth.spline(p[ok], SA[ok], df=df), p)$y
            CT <- predict(smooth.spline(p[ok], CT[ok], df=df), p)$y
        }
        latitude <- ctd[["latitude"]]
        if (is.na(latitude))
            latitude <- 0
        l <- gsw::gsw_Nsquared(SA=SA, CT=CT, p=p, latitude=latitude)
        ## approx back to the given pressures
        res <- approx(l$p_mid, l$N2, p, rule=2)$y
    }
    res
}


#' Water pressure
#'
#' Compute seawater pressure from depth by inverting \code{\link{swDepth}}
#' using \code{\link{uniroot}}.
#'
#' If \code{eos="unesco"} this is done by numerical inversion of
#' \code{\link{swDepth}} is done using \code{\link{uniroot}}. If
#' \code{eos="gsw"}, it is done using \code{\link[gsw]{gsw_p_from_z}} in the
#' \code{gsw} package.
#'
#' @param depth distance below the surface in metres.
#' @param latitude Latitude in \eqn{^\circ}{deg}N or radians north of the
#' equator.
#' @param eos indication of formulation to be used, either \code{"unesco"} or
#' \code{"gsw"}.
#' @return Pressure in dbar.
#' @author Dan Kelley
#' @references Unesco 1983. Algorithms for computation of fundamental
#' properties of seawater, 1983. \emph{Unesco Tech. Pap. in Mar. Sci.}, No. 44,
#' 53 pp.
#' @examples
#' swPressure(9712.653, 30, eos="unesco") # 10000
#' swPressure(9712.653, 30, eos="gsw")    #  9998.863
#'
#' @family functions that calculate seawater properties
swPressure <- function(depth, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw add gsw version
    ndepth <- length(depth)
    if (length(latitude) < ndepth)
        latitude <- rep(latitude, ndepth)
    res <- vector("numeric", ndepth)
    eos <- match.arg(eos, c("unesco", "gsw"))
    ## Takes 3.55s for 15225 points
    if (eos == "unesco") {
        for (i in 1:ndepth) {          # FIXME: this loop is slow and should be done in C, like swCStp()
            res[i] <- if (depth[i] == 0) 0 else
                uniroot(function(p) depth[i] - swDepth(p, latitude[i], eos), interval=depth[i]*c(0.9, 1.1))$root
        }
    } else if (eos == "gsw") {
        res <- gsw::gsw_p_from_z(-depth, latitude)
    } else {
        stop("eos must be 'unesco' or 'gsw'")
    }
    res
}


#' Electrical conductivity ratio from salinity, temperature and pressure
#'
#' Compute electrical conductivity ratio based on salinity, temperature, and
#' pressure (relative to the conductivity of seawater with salinity=35,
#' temperature68=15, and pressure=0).
#'
#' If \code{eos="unesco"}, the calculation is done by a bisection root search
#' on the UNESCO formula relating salinity to conductivity, temperature, and
#' pressure (see \code{\link{swSCTp}}).  If it is \code{"gsw"} then the
#' Gibbs-SeaWater formulation is used, via \code{\link{gsw_C_from_SP}}.
#'
#' @param salinity practical salinity, or a CTD object (in which case its
#' temperature and pressure are used, and the next two arguments are ignored)
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Conductivity ratio [unitless], i.e. the ratio of conductivity to the
#' conductivity at salinity=35, temperature=15 (IPTS-68 scale) and pressure=0,
#' which has numerical value 42.9140 (see Culkin and Smith, 1980).
#' @author Dan Kelley
#' @seealso For thermal (as opposed to electrical) conductivity, see
#' \code{\link{swThermalConductivity}}. For computation of salinity from
#' electrical conductivity, see \code{\link{swSCTp}}.
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp.
#'
#' 2. Culkin, F., and Norman D. Smith, 1980. Determination of the concentration of
#' potassium chloride solution having the same electrical conductivity, at 15 C
#' and infinite frequency, as standard seawater of salinity 35.0000 ppt
#' (Chlorinity 19.37394 ppt). \emph{IEEE Journal of Oceanic Engineering},
#' \bold{5}, pp 22-23.
#' @examples
#' swCSTp(35, T90fromT68(15), 0, eos="unesco") # 1, by definition of cond. ratio
#' swCSTp(34.25045, T90fromT68(15), 2000, eos="unesco") # 1
#' swCSTp(34.25045, 15, 2000, eos="unesco") # 1.000081
#' swCSTp(34.25045, 15, 2000, eos="gsw")  # 0.9999982
#'
#' @family functions that calculate seawater properties
swCSTp <- function(salinity=35, temperature=15, pressure=0,
                   eos=getOption("oceEOS", default="gsw"))

{
    if (missing(salinity)) stop("must supply salinity (which may be S or a CTD object)")
    if (inherits(salinity, "oce")) {
        ctd <- salinity
        salinity <- ctd[["salinity"]]
        temperature <- ctd[["temperature"]]
        pressure <- ctd[["pressure"]]
    }
    dim <- dim(salinity)
    eos <- match.arg(eos, c("unesco", "gsw"))
    if (eos == "unesco") {
        n <- length(salinity)
        res <- .C("sw_CSTp", as.integer(n), as.double(salinity), T68fromT90(as.double(temperature)), as.double(pressure), C=double(n), NAOK=TRUE, PACKAGE="oce")$C
    } else {
        ## for the use of a constant, as opposed to a function call with (35,15,0), see
        ## https://github.com/dankelley/oce/issues/746
        res <- gsw::gsw_C_from_SP(SP=salinity, t=temperature, p=pressure) / 42.9140
    }
    dim(res) <- dim
    res
}


#' Salinity from electrical conductivity, temperature and pressure
#'
#' Compute salinity based on electrical conductivity, temperature, and
#' pressure.
#'
#' Calculate salinity from what is actually measured by a CTD, \emph{i.e.}
#' conductivity, \emph{in-situ} temperature and pressure.  Often this is done
#' by the CTD processing software, but sometimes it is helpful to do this
#' directly, \emph{e.g.} when there is a concern about mismatches in sensor
#' response times.  If \code{eos="unesco"} then salinity is calculated using
#' the UNESCO algorithm described by Fofonoff and Millard (1983); if it is
#' \code{"gsw"} then the Gibbs-SeaWater formulation is used, via
#' \code{\link{gsw_SP_from_C}}.
#'
#' @param conductivity a measure of conductivity (see also \code{conductivityUnit})
#' or an \code{oce} object holding hydrographic information. In the second case,
#' all the other arguments to \code{swSCTp} are ignored.
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @param conductivityUnit string indicating the unit used for conductivity.
#' This may be \code{"ratio"} or \code{""} (meaning conductivity ratio),
#' \code{"mS/cm"} or \code{"S/m"}.  Note that the ratio mode assumes that
#' measured conductivity has been divided by the standard conductivity
#' of 4.2914 S/m.
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Practical salinity.
#' @author Dan Kelley
#' @seealso For thermal (as opposed to electrical) conductivity, see
#' \code{\link{swThermalConductivity}}.  For computation of electrical
#' conductivity from salinity, see \code{\link{swCSTp}}.
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp
#' @examples
#' swSCTp(1, T90fromT68(15), 0, eos="unesco") # 35
#' swSCTp( 1,            15, 0, eos="gsw") # 35
#' 
#' @family functions that calculate seawater properties
swSCTp <- function(conductivity, temperature=NULL, pressure=0,
                   conductivityUnit=c("", "mS/cm", "S/m"),
                   eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw add gsw version
    if (missing(conductivity)) stop("must supply conductivity (which may be S or a CTD object)")
    if (is.list(conductivityUnit) && "unit" %in% names(conductivityUnit))
        conductivityUnit <- conductivityUnit$unit
    if (is.expression(conductivityUnit))
        conductivityUnit <- as.character(conductivityUnit)
    conductivityUnit <- match.arg(conductivityUnit)
    if (inherits(conductivity, "oce")) {
        if (inherits(conductivity, "rsk")) {
            ctd <- as.ctd(conductivity)
        } else {
            ctd <- conductivity
        }
        conductivity <- ctd[["conductivity"]]
        if (is.null(conductivity)) stop("this CTD object has no conductivity")
        ## Use unit within the object, ignoring the argument supplied here (FIXME: is this best?)
        tmp <- ctd[["conductivityUnit"]]
        if (is.list(tmp) && "unit" %in% names(tmp))
            conductivityUnit <- as.character(tmp$unit)
        temperature <- ctd[["temperature"]]
        pressure <- ctd[["pressure"]]
    }
    if (is.list(conductivityUnit)) {
        conductivityUnit <- as.character(conductivityUnit$unit)
    }
    if (conductivityUnit == "mS/cm")
        conductivity <- conductivity / 42.914
    else if (conductivityUnit == "S/m")
        conductivity <- conductivity / 4.2914
    else
        conductivity <- conductivity
    ## Now, "conductivity" is in ratio form
    dim <- dim(conductivity)
    nC <- length(conductivity)
    nT <- length(temperature)
    if (nC != nT)
        stop("lengths of conductivity and temperature must agree, but they are ", nC, " and ", nT)
    if (missing(pressure))
        pressure <- rep(0, nC)
    np <- length(pressure)
    if (nC != np)
        stop("lengths of conductivity and pressure must agree, but they are ", nC, " and ", np)
    if (eos == "unesco") {
        res <- .C("sw_salinity",
                   as.integer(nC),
                   as.double(conductivity),
                   as.double(T68fromT90(temperature)),
                   as.double(pressure),
                   value = double(nC),
                   NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        C0 <- gsw::gsw_C_from_SP(35, 15, 0)
        res <- gsw::gsw_SP_from_C(C0 * conductivity, temperature, pressure)
    }
    dim(res) <- dim
    res
}


#' Seawater salinity from temperature and density
#'
#' Compute Practical or Absolute Salinity, given in-situ or Conservative
#' Temperature, density, and pressure. This is mainly used to draw isopycnal
#' lines on TS diagrams, hence the dual meanings for salinity and temperature,
#' depending on the value of \code{eos}.
#'
#' For \code{eos="unesco"}, finds the practical salinity that yields the given
#' density, with the given in-situ temperature and pressure.  The method is a
#' bisection search with a salinity tolerance of 0.001.  For \code{eos="gsw"},
#' the function \code{\link[gsw]{gsw_SA_from_rho}} in the \code{gsw}
#' package is used
#' to infer Absolute Salinity from Conservative Temperature.
#'
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param density \emph{in-situ} density or sigma value [\eqn{kg/m^3}{kg/m^3}]
#' @param pressure \emph{in-situ} pressure [dbar]
#' @param eos equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
#' [3,4].
#' @return Practical Salinity, if \code{eos="unesco"}, or Absolute Salinity, if
#' \code{eos="gsw"}.
#' @author Dan Kelley
#' @seealso \code{\link{swTSrho}}
#' @references
#'
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
#' Science}, \bold{44}, 53 pp
#' 
#' 2. Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
#' York, 662 pp.
#' 
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 4. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#' @examples
#' swSTrho(10, 22, 0, eos="gsw") # 28.76285
#' swSTrho(10, 22, 0, eos="unesco") # 28.651625
#' 
#' @family functions that calculate seawater properties
swSTrho <- function(temperature, density, pressure, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw add gsw version
    eos <- match.arg(eos, c("unesco", "gsw"))
    teos <- eos == "gsw" # FIXME still the best way?
    dim <- dim(temperature)
    nt <- length(temperature)
    nrho <- length(density)
    np <- length(pressure)
    if (nrho == 1) density <- rep(density, nt)
    if (np == 1) pressure <- rep(pressure, nt)
    if (nt == 1) temperature <- rep(temperature, nt)
    sigma <- ifelse(density > 500, density - 1000, density)
    if (eos == "unesco") {
        res <- .C("sw_strho",
                   as.integer(nt),
                   as.double(T68fromT90(temperature)),
                   as.double(sigma),
                   as.double(pressure),
                   as.integer(teos),
                   S=double(nt),
                   NAOK=TRUE, PACKAGE="oce")$S
                   ##NAOK=TRUE)$S # permits dyn.load() on changing .so
    } else if (eos == "gsw") {
        density <- ifelse(density < 900, density + 1000, density)
        res <- gsw::gsw_SA_from_rho(density, temperature, pressure) ## assumes temperature=CT
    }
    dim(res) <- dim
    res
}


#' Seawater temperature from salinity and density
#'
#' Compute \emph{in-situ} temperature, given salinity, density, and pressure.
#'
#' Finds the temperature that yields the given density, with the given salinity
#' and pressure.  The method is a bisection search with temperature tolerance
#' 0.001 \eqn{^\circ C}{degC}.
#'
#' @param salinity \emph{in-situ} salinity [PSU]
#' @param density \emph{in-situ} density or sigma value [kg/m\eqn{^3}{^3}]
#' @param pressure \emph{in-situ} pressure [dbar]
#' @param eos equation of state to be used, either \code{"unesco"} or
#' \code{"gsw"} (ignored at present).
#' @return \emph{In-situ} temperature [\eqn{^\circ C}{degC}] in the ITS-90
#' scale.
#' @author Dan Kelley
#' @seealso \code{\link{swSTrho}}
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp
#'
#' Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
#' York, 662 pp.
#' @examples
#' swTSrho(35, 23, 0, eos="unesco") # 26.11301
#'
#' @family functions that calculate seawater properties
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
        this.T <- T90fromT68(this.T)
    	if (i == 1) res <- this.T else res <- c(res, this.T)
    }
    dim(res) <- dim
    res
}


#' Seawater freezing temperature
#'
#' Compute freezing temperature of seawater.
#'
#' In the first form, the argument is a \code{ctd} object, from which the
#' salinity and pressure values are extracted and used to for the calculation.
#'
#' @param salinity either salinity [PSU] or a \code{ctd} object from which
#' salinity will be inferred.
#' @param pressure seawater pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param saturation_fraction saturation fraction of dissolved air in seawater
#' (used only if \code{eos="gsw"}).
#' @param eos equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
#' [3,4].
#' @return Temperature [\eqn{^\circ}{deg}C], defined on the ITS-90 scale.
#' @author Dan Kelley
#' @references [1] Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp
#'
#' [2] Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
#' York, 662 pp.
#'
#' [3] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' [4] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#' @examples
#' swTFreeze(salinity=40, pressure=500, eos="unesco") # -2.588567 degC
#'
#' @family functions that calculate seawater properties
swTFreeze <- function(salinity, pressure=0,
                      longitude=300, latitude=30, saturation_fraction=1,
                      eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must supply salinity (which may be S or a CTD object)")
    l <- lookWithin(list(salinity=salinity, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    Smatrix <- is.matrix(l$salinity)
    dim <- dim(l$salinity)
    ##n <- length(l$salinity)
    if (eos == "unesco") {
        res <- (-.0575+1.710523e-3*sqrt(abs(l$salinity))-2.154996e-4*l$salinity)*l$salinity-7.53e-4*l$pressure
        res <- T90fromT68(res)
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        res <- gsw::gsw_t_freezing(SA=SA, p=l$pressure, saturation_fraction=1)
    }
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater thermal expansion coefficient
#'
#' Compute \eqn{\alpha}{alpha}, the thermal expansion coefficient for seawater.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Value in 1/degC.
#' @author Dan Kelley
#' @references The \code{eos="unesco"} formulae are based on the UNESCO
#' equation of state, but are formulated empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The \code{eos="gsw"} formulae come from GSW; see references in
#' the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swAlpha <- function(salinity, temperature=NULL, pressure=0,
                    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (l$eos == "unesco") {
        res <- swAlphaOverBeta(l$salinity, l$temperature, l$pressure, eos="unesco") * swBeta(l$salinity, l$temperature, l$pressure, eos="unesco")
    } else if (l$eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_alpha(SA=SA, CT=CT, p=l$pressure)
    }
    res
}


#' Ratio of seawater thermal expansion coefficient to haline contraction
#' coefficient
#'
#' Compute \eqn{\alpha/\beta}{alpha/beta} using McDougall's (1987) algorithm.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C]
#' @param pressure pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Value in psu/\eqn{^\circ}{deg}C.
#' @author Dan Kelley
#' @references The \code{eos="unesco"} formulae are based on the UNESCO
#' equation of state, but are formulaed empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The \code{eos="gsw"} formulae come from GSW; see references in
#' the \code{\link{swRho}} documentation.
#' @examples
#' swAlphaOverBeta(40, 10, 4000, eos="unesco") # 0.3476
#'
#' @family functions that calculate seawater properties
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
    if (l$eos == "unesco") {
        theta <- swTheta(l$salinity, l$temperature, l$pressure)
        res <- .C("sw_alpha_over_beta", as.integer(nS),
                   as.double(l$salinity), as.double(theta), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (l$eos == "gsw") {
        ## not likely to be called since gsw has a direct function for alpha, but put this here anyway
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        alpha <- gsw::gsw_alpha(SA=SA, CT=CT, p=l$pressure)
        beta <- gsw::gsw_beta(SA=SA, CT=CT, p=l$pressure)
        res <- alpha / beta
    }
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater haline contraction coefficient
#'
#' Compute \eqn{\beta}{beta}, the haline contraction coefficient for seawater.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure seawater pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Value in 1/psu.
#' @author Dan Kelley
#' @references The \code{eos="unesco"} formulae are based on the UNESCO
#' equation of state, but are formulaed empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The \code{eos="gsw"} formulae come from GSW; see references in
#' the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swBeta <- function(salinity, temperature=NULL, pressure=0,
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
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (l$eos == "unesco") {
        theta <- swTheta(l$salinity, l$temperature, l$pressure, eos=l$eos) # the formula is i.t.o. theta
        res <- .C("sw_beta", as.integer(nS),
                   as.double(l$salinity), as.double(theta), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (l$eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_beta(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}

## thermal (not electrical) conductivity, using Caldwell (1974) as of 2015-jan-9
## NOTE: no gsw equivalent


#' Seawater thermal conductivity
#'
#' Compute seawater thermal conductivity, in \eqn{W
#' m^{-1\circ}C^{-1}}{W/(m*degC)}
#'
#' Caldwell's (1974) detailed formulation is used.  To be specific, his
#' equation 6 to calculate K, and his two sentences above that equation are
#' used to infer this to be K(0,T,S) in his notation of equation 7. Then,
#' application of his equations 7 and 8 is straightforward. He states an
#' accuracy for this method of 0.3 percent.  (See the check against his Table 1
#' in the \dQuote{Examples}.)
#'
#' @param salinity salinity [PSU], or a \code{ctd} object, in which case
#' \code{temperature} and \code{pressure} will be ignored.
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @return Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.
#'
#' To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide by the
#' product of density and specific heat, as in the example.
#' @author Dan Kelley
#' @references Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
#' \emph{Deep-sea Research}, \bold{21}, 131-137.
#' @examples
#'
#' library(oce)
#' # Values in m^2/s, a unit that is often used instead of W/(m*degC).
#' swThermalConductivity(35, 10, 100) / (swRho(35,10,100) * swSpecificHeat(35,10,100)) # ocean
#' swThermalConductivity(0, 20, 0) / (swRho(0, 20, 0) * swSpecificHeat(0, 20, 0)) # lab
#' # Caldwell Table 1 gives 1478e-6 cal/(cm*sec*degC) at 31.5 o/oo, 10degC, 1kbar
#' joulePerCalorie <- 4.18400
#' cmPerM <- 100
#' swThermalConductivity(31.5,10,1000) / joulePerCalorie / cmPerM
#'
#' @family functions that calculate seawater properties
swThermalConductivity <- function (salinity, temperature=NULL, pressure=NULL)
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure))
    ## below is formula used prior to 2015-jan-9
    ## return(0.57057 * (1 + l$temperature * (0.003 - 1.025e-05 * l$temperature) + 0.000653 * l$pressure - 0.00029 * l$salinity))
    S <- l$salinity
    T <- T68fromT90(l$temperature)
    p <- l$pressure / 1e3              # Caldwell formula is for kbar, not dbar
    if (TRUE) {
        K <- 1.3507e-3 + 4.061e-6*T + (-1.412e-8)*T^2 # Caldwell eq 6
        f <- 0.0690 + (-8e-5)*T + (-0.0020)*p + (-0.00010)*S # Caldwell eq 8
        cond <- K * (1 + f)                # Caldwell eq 7
    } else {
        cond <- 0.001365*(1+0.003*T - 1.025e-5*T^2 + 0.0653 * p - 0.00029 * S)
    }
    418.400 * cond                     # convert from cal/(cm*sec*degC) to J/(m*sec*degC)
}


#' Water depth
#'
#' Compute depth below the surface (i.e. a positive number within the
#' water column) based on pressure and latitude. (Use \code{\link{swZ}}
#' to get the vertical coordinate, which is negative within the water
#' column.)
#'
#' If \code{eos="unesco"} then depth is calculated from pressure using Saunders
#' and Fofonoff's method, with the formula refitted for 1980 UNESCO equation of
#' state [1].  If \code{eos="gsw"}, then \code{\link[gsw]{gsw_z_from_p}} from
#' the \code{gsw} package [2,3] is used.
#'
#' @param pressure either pressure [dbar], in which case \code{lat} must also
#' be given, or a \code{ctd} object, in which case \code{lat} will be inferred
#' from the object.
#' @param latitude Latitude in \eqn{^\circ}{deg}N or radians north of the
#' equator.
#' @param eos indication of formulation to be used, either \code{"unesco"} or
#' \code{"gsw"}.
#' @return Depth below the ocean surface, in metres.
#' @author Dan Kelley
#' @references
#' 1. Unesco 1983. Algorithms for computation of fundamental
#' properties of seawater, 1983. \emph{Unesco Tech. Pap. in Mar. Sci.}, No. 44,
#' 53 pp.
#'
#' 2. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 3. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#' @examples
#' d <- swDepth(10, 45)
#'
#' @family functions that calculate seawater properties
swDepth <- function(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure)) stop("must provide pressure")
    l <- lookWithin(list(pressure=pressure, latitude=latitude, eos=eos))
    if (any(is.na(l$latitude)))
        l$latitude <- 45 # default to mid latitudes
    if (l$eos == "unesco") {
        l$latitude <- l$latitude * atan2(1, 1) / 45
        x <- sin(l$latitude)^2
        gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*l$pressure
        res <- (((-1.82e-15*l$pressure+2.279e-10)*l$pressure-2.2512e-5)*l$pressure+9.72659)*l$pressure / gr
    } else if (l$eos == "gsw") {
        res <- -gsw::gsw_z_from_p(p=l$pressure, latitude=l$latitude)
    }
    res
}

#' Vertical coordinate
#'
#' Compute height above the surface. This is the negative of depth,
#' and so is defined simply in terms of \code{\link{swDepth}}.
#' @inheritParams swDepth
#' @family functions that calculate seawater properties
swZ <- function(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure)) stop("must provide pressure")
    -swDepth(pressure=pressure, latitude=latitude, eos=eos)
}


#' Dynamic height of seawater profile
#'
#' Compute the dynamic height of a column of seawater.
#'
#' If the first argument is a \code{section}, then dynamic height is calculated
#' for each station within a section, and returns a list containing distance
#' along the section along with dynamic height.
#'
#' If the first argument is a \code{ctd}, then this returns just a single
#' value, the dynamic height.
#'
#' If \code{eos="unesco"}, processing is as follows.  First, a piecewise-linear
#' model of the density variation with pressure is developed using
#' \code{\link[stats]{approxfun}}. (The option \code{rule=2} is used to
#' extrapolate the uppermost density up to the surface, preventing a possible a
#' bias for bottle data, in which the first depth may be a few metres below the
#' surface.) A second function is constructed as the density of water with
#' salinity 35PSU, temperature of 0\eqn{^\circ}{deg}C, and pressure as in the
#' \code{ctd}. The difference of the reciprocals of these densities, is then
#' integrated with \code{\link[stats]{integrate}} with pressure limits \code{0}
#' to \code{referencePressure}.  (For improved numerical results, the variables
#' are scaled before the integration, making both independent and dependent
#' variables be of order one.)
#'
#' NOTE: As of early 2015, the \code{eos="gsw"} case is handled exactly the
#' same as the \code{"unesco"} case, because the GSW C code (version 3.0.3)
#' lacks the requisite functions.
#'
#' @param x a \code{section} object, \strong{or} a \code{ctd} object.
#' @param referencePressure reference pressure [dbar]
#' @param subdivisions number of subdivisions for call to
#' \code{\link{integrate}}.  (The default value is considerably larger than the
#' default for \code{\link{integrate}}, because otherwise some test profiles
#' failed to integrate.
#' @param rel.tol absolute tolerance for call to \code{\link{integrate}}.  Note
#' that this call is made in scaled coordinates, i.e. pressure is divided by
#' its maximum value, and dz/dp is also divided by its maximum.
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return In the first form, a list containing \code{distance}, the distance
#' [km] from the first station in the section and \code{height}, the dynamic
#' height [m].
#'
#' In the second form, a single value, containing the dynamic height [m].
#' @author Dan Kelley
#' @references Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic
#' Press, New York, 662 pp.
#' @examples
#' \dontrun{
#' library(oce)
#' data(section)
#'
#' # Dynamic height and geostrophy
#' par(mfcol=c(2,2))
#' par(mar=c(4.5,4.5,2,1))
#'
#' # Left-hand column: whole section
#' # (The smoothing lowers Gulf Stream speed greatly)
#' westToEast <- subset(section, 1<=stationId&stationId<=123)
#' dh <- swDynamicHeight(westToEast)
#' plot(dh$distance, dh$height, type='p', xlab="", ylab="dyn. height [m]")
#' ok <- !is.na(dh$height)
#' smu <- supsmu(dh$distance, dh$height)
#' lines(smu, col="blue")
#' f <- coriolis(section[["station", 1]][["latitude"]])
#' g <- gravity(section[["station", 1]][["latitude"]])
#' v <- diff(smu$y)/diff(smu$x) * g / f / 1e3 # 1e3 converts to m
#' plot(smu$x[-1], v, type='l', col="blue", xlab="distance [km]", ylab="velocity [m/s]")
#'
#' # right-hand column: gulf stream region, unsmoothed
#' gs <- subset(section, 102<=stationId&stationId<=124)
#' dh.gs <- swDynamicHeight(gs)
#' plot(dh.gs$distance, dh.gs$height, type='b', xlab="", ylab="dyn. height [m]")
#' v <- diff(dh.gs$height)/diff(dh.gs$distance) * g / f / 1e3
#' plot(dh.gs$distance[-1], v, type='l', col="blue",
#'   xlab="distance [km]", ylab="velocity [m/s]")
#' }
#'
#' @family functions that calculate seawater properties
swDynamicHeight <- function(x, referencePressure=2000,
                            subdivisions=500, rel.tol=.Machine$double.eps^0.25,
                            eos=getOption("oceEOS", default="gsw"))
{
    eos <- match.arg(eos, c("unesco", "gsw"))
    if (eos == "gsw")
        warning("using unesco because gsw toolbox v3.03 lacks dynamic height calculation")
    height <- function(ctd, referencePressure, subdivisions, rel.tol, eos=getOption("oceEOS", default="gsw"))
    {
        if (sum(!is.na(ctd@data$pressure)) < 2) return(NA) # cannot integrate then
        ## 2015-Jan-10: the C library does not have gsw::gsw_geo_strf_dyn_height() as of vsn 3.0.3
        #if (eos == "unesco") {
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
            res <- integrate(integrand, 0, 1,
                              subdivisions=subdivisions, rel.tol=rel.tol)$value * referencePressure * max
        #} else if (eos == "gsw") {
        #     SA <- ctd[["SA"]]
        #     CT <- ctd[["CT"]]
        #     p <- ctd[["p"]]
        #     res <- gsw::gsw_geo_strf_dyn_height(SA=SA, CT=CT, p=p, p_ref=referencePressure)
        #}
        res
    }
    if (inherits(x, "section")) {
        lon0 <- x@data$station[[1]]@metadata$longitude
        lat0 <- x@data$station[[1]]@metadata$latitude
        ns <- length(x@data$station)
        d <- vector("numeric", ns)
        h <- vector("numeric", ns)
        for (i in 1:ns) {
            d[i] <- geodDist(x@data$station[[i]]@metadata$longitude, x@data$station[[i]]@metadata$latitude, lon0, lat0)
            h[i] <- height(x@data$station[[i]], referencePressure, subdivisions=subdivisions, rel.tol=rel.tol, eos=eos)
        }
        return(list(distance=d, height=h))
    } else if (inherits(x, "ctd")) {
        return(height(x, referencePressure, subdivisions=subdivisions, rel.tol=rel.tol))
    } else {
        stop("method is only for objects of class '", "section", " or '", "ctd", "'")
    }
}


#' Seawater lapse rate
#'
#' Compute adiabatic lapse rate
#'
#' If \code{eos="unesco"}, the density is calculated using the UNESCO equation
#' of state for seawater [1,2], and if \code{eos="gsw"}, the GSW formulation
#' [3,4] is used.
#'
#' @param salinity either salinity [PSU] (in which case \code{temperature} and
#' \code{pressure} must be provided) \strong{or} a \code{ctd} object (in which
#' case \code{salinity}, \code{temperature} and \code{pressure} are determined
#' from the object, and must not be provided in the argument list).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
#' [3,4].
#' @return Lapse rate [\eqn{deg}{deg}C/m].
#' @author Dan Kelley
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp.  (Section 7, pages 38-40)
#' @examples
#' lr <- swLapseRate(40, 40, 10000) # 3.255976e-4
#'
#' @family functions that calculate seawater properties
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
        res <- .C("sw_lapserate", as.integer(nS), as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        ## the 1e4 is to convert from 1/Pa to 1/dbar
        res<- 1e4 * gsw::gsw_adiabatic_lapse_rate_from_CT(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}                                      # swLapseRate


#' Seawater density
#'
#' Compute \eqn{\rho}{rho}, the \emph{in-situ} density of seawater.
#'
#' If \code{eos="unesco"}, the density is calculated using the UNESCO equation
#' of state for seawater [1,2], and if \code{eos="gsw"}, the GSW formulation
#' [3,4] is used.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object, in
#' which case \code{salinity}, \code{temperature} (in the ITS-90 scale; see
#' next item), etc. are inferred from the object.
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale.  This scale is used by GSW-style calculation (as
#' requested by setting \code{eos="gsw"}), and is the value contained within
#' \code{ctd} objects (and probably most other objects created with data
#' acquired in the past decade or two). Since the UNESCO-style calculation is
#' based on IPTS-68, the temperature is converted within the present function,
#' using \code{\link{T68fromT90}}.
#' @param pressure pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
#' [3,4].
#' @return \emph{In-situ} density [kg/m\eqn{^3}{^3}].
#' @section Temperature units: The UNESCO formulae are defined in terms of
#' temperature measured on the IPTS-68 scale, whereas the replacement GSW
#' formulae are based on the ITS-90 scale. Prior to the addition of GSW
#' capabilities, the various \code{sw*} functions took temperature to be in
#' IPTS-68 units. As GSW capabilities were added in early 2015, the assumed
#' unit of \code{temperature} was taken to be ITS-90.  This change means that
#' old code has to be modified, by replacing e.g. \code{swRho(S, T, p)} with
#' \code{swRho(S, T90fromT68(T), p)}. At typical oceanic values, the difference
#' between the two scales is a few millidegrees.
#' @author Dan Kelley
#' @seealso Related density routines include \code{\link{swSigma0}} (and
#' equivalents at other pressure horizons), \code{\link{swSigmaT}}, and
#' \code{\link{swSigmaTheta}}.
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
#' Science}, \bold{44}, 53 pp
#'
#' 2. Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
#' York, 662 pp.
#'
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 4. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#' @examples
#' library(oce)
#' rho <- swRho(35, 13, 1000)
#'
#' @family functions that calculate seawater properties
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
        res <- .C("sw_rho", as.integer(nS), as.double(l$salinity),
                   as.double(T68fromT90(l$temperature)),
                   as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_rho(SA, CT, p=l$pressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater density anomaly
#'
#' Compute \eqn{\sigma_\theta}{sigma}, the density of seawater, minus 1000
#' kg/m\eqn{^3}{^3}.
#'
#' @inheritParams swRho
#' @return Density anomaly [kg/m\eqn{^3}{^3}], defined as \code{\link{swRho}} -
#' 1000 kg/m\eqn{^3}{^3}.
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @examples
#' library(oce)
#' swSigma(35, 13, 1000, eos="gsw")                   # 30.82397
#' swSigma(35, T90fromT68(13), 1000, eos="unesco")    # 30.8183
#'
#' @family functions that calculate seawater properties
swSigma <- function(salinity, temperature=NULL, pressure=NULL,
                    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    swRho(salinity, temperature, pressure,
          longitude=longitude, latitude=latitude, eos=eos) - 1000
}


#' Seawater quasi-potential density anomaly
#'
#' Compute \eqn{\sigma_t}{sigma-t}, a rough estimate of potential density of
#' seawater, minus 1000 kg/m\eqn{^3}{^3}.
#'
#' If the first argument is an \code{oce} object, then salinity, etc., are
#' extracted from it, and used for the calculation.
#'
#' @inheritParams swRho
#' @return Quasi-potential density anomaly [kg/m\eqn{^3}{^3}], defined as the
#' density calculated with pressure set to zero.
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @examples
#' swSigmaT(35, 13, 1000, eos="gsw")                # 26.396816
#' swSigmaT(35, T90fromT68(13), 1000, eos="unesco") # 26.393538
#' 
#' @family functions that calculate seawater properties
swSigmaT <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity)) stop("must provide salinity")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude, eos=eos))
    swRho(l$salinity, l$temperature, pressure=rep(0, length(l$salinity)),
          longitude=l$longitude, latitude=l$latitude, eos=l$eos) - 1000
}


#' Seawater potential density anomaly
#'
#' Compute \eqn{\sigma_\theta}{sigma-theta}, the potential density of seawater,
#' minus 1000 kg/m\eqn{^3}{^3}.
#'
#' If the first argument is an \code{oce} object, then salinity, etc., are
#' extracted from it, and used for the calculation instead of any values
#' provided in the other arguments.
#' 
#' @inheritParams swRho
#' @param referencePressure The reference pressure, in dbar.
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}], defined as
#' \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
#' - 1000 kg/m\eqn{^3}{^3}.
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @examples
#' swSigmaTheta(35, 13, 1000)             # 26.42514 (gsw)
#' swSigmaTheta(35, 13, 1000, eos="unesco") # 26.4219
#' 
#' @family functions that calculate seawater properties
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



#' Seawater potential density anomaly referenced to surface pressure
#' 
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to surface pressure.
#' 
#' Definition:
#' \eqn{\sigma_0=\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_0=sigma_theta=rho(S,theta(S,t,p),0)}
#' - 1000 kg/m\eqn{^3}{^3}.
#' 
#' @inheritParams swRho
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @examples
#' swSigma0(35, 13, 1000)
#' @family functions that calculate seawater properties
swSigma0 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=0,
                 longitude=longitude, latitude=latitude, eos=eos)
}

#' Seawater potential density anomaly referenced to 1000db pressure
#' 
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to 1000db pressure.
#' 
#' Definition:
#' \eqn{\sigma_1=\sigma_\theta=\rho(S,\theta(S,t,p),1000}{sigma_1=sigma_theta=rho(S,theta(S,t,p),1000)}
#' - 1000 kg/m\eqn{^3}{^3}.
#' 
#' @inheritParams swRho
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swSigma1 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=1000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

#' Seawater potential density anomaly referenced to 2000db pressure
#' 
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to 2000db pressure.
#' 
#' Definition:
#' \eqn{\sigma_1=\sigma_\theta=\rho(S,\theta(S,t,p),1000}{sigma_1=sigma_theta=rho(S,theta(S,t,p),2000)}
#' - 2000 kg/m\eqn{^3}{^3}.
#' 
#' @inheritParams swRho
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swSigma2 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=2000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

#' Seawater potential density anomaly referenced to 3000db pressure
#' 
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to 3000db pressure.
#' 
#' Definition:
#' \eqn{\sigma_1=\sigma_\theta=\rho(S,\theta(S,t,p),3000}{sigma_1=sigma_theta=rho(S,theta(S,t,p),3000)}
#' - 1000 kg/m\eqn{^3}{^3}.
#' 
#' @inheritParams swRho
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swSigma3 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=3000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

#' Seawater potential density anomaly referenced to 4000db pressure
#' 
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to 4000db pressures.
#' 
#' Definition:
#' \eqn{\sigma_1=\sigma_\theta=\rho(S,\theta(S,t,p),4000}{sigma_1=sigma_theta=rho(S,theta(S,t,p),4000)}
#' - 1000 kg/m\eqn{^3}{^3}.
#' 
#' @inheritParams swRho
#' @return Potential density anomaly [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references See citations provided in the \code{\link{swRho}} documentation.
#' @family functions that calculate seawater properties
swSigma4 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity, temperature, pressure, referencePressure=4000,
                 longitude=longitude, latitude=latitude, eos=eos)
}


#' Seawater sound absorption in dB/m
#'
#' Compute the sound absorption of seawater, in dB/m
#'
#' Salinity and pH are ignored in this formulation.  Several formulae may be
#' found in the literature, and they give results differing by 10 percent, as
#' shown at [3] for example.  For this reason, it is likely that more
#' formulations will be added to this function, and entirely possible that the
#' default may change.
#' 
#' @inheritParams swRho
#' @param frequency The frequency of sound, in Hz.
#' @param formulation character string indicating the formulation to use, either
#' of \code{"fischer-simmons"} or \code{"francois-garrison"}; see \dQuote{References}.
#' @param pH seawater pH
#' @return Sound absorption in dB/m.
#' @author Dan Kelley
#' @references
#' 1. F. H. Fisher and V. P. Simmons, 1977.  Sound absorption in
#' sea water.  J. Acoust. Soc. Am., 62(3), 558-564.
#' 
#' 2. R. E. Francois and G. R. Garrison, 1982.  Sound absorption based on
#' ocean measurements.  Part II: Boric acid contribution and equation for total
#' absorption.  J. Acoust. Soc. Am., 72(6):1879-1890.
#' 
#' 3. \url{http://resource.npl.co.uk/acoustics/techguides/seaabsorption/}
#' @examples
#' ## Fisher & Simmons (1977 table IV) gives 0.52 dB/km for 35 PSU, 5 degC, 500 atm
#' ## (4990 dbar of water)a and 10 kHz
#' alpha <- swSoundAbsorption(35, 4, 4990, 10e3)
#' 
#' ## reproduce part of Fig 8 of Francois and Garrison (1982 Fig 8)
#' f <- 1e3 * 10^(seq(-1,3,0.1)) # in KHz
#' plot(f/1000, 1e3*swSoundAbsorption(f, 35, 10, 0, formulation='fr'),
#'      xlab=" Freq [kHz]", ylab=" dB/km", type='l', log='xy')
#' lines(f/1000, 1e3*swSoundAbsorption(f, 0, 10, 0, formulation='fr'), lty='dashed')
#' legend("topleft", lty=c("solid", "dashed"), legend=c("S=35", "S=0"))
#' 
#' @family functions that calculate seawater properties
swSoundAbsorption <- function(frequency, salinity, temperature, pressure, pH=8,
                              formulation=c("fisher-simmons", "francois-garrison"))
{
    formulation <- match.arg(formulation)
    if (formulation == "fisher-simmons") {
        ## Equation numbers are from Fisher & Simmons (1977); see help page for ref
        p <- 1 + pressure / 10  # add atmophere, then convert water part from dbar
        S <- salinity
        T <- T68fromT90(temperature)
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
        T <- T68fromT90(temperature)
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


#' Seawater sound speed
#'
#' Compute the seawater speed of sound.
#'
#' If \code{eos="unesco"}, the sound speed is calculated using the formulation
#' in section 9 of Fofonoff and Millard (1983). If \code{eos="gsw"}, then the
#' \code{\link[gsw]{gsw_sound_speed}} function from the
#' \code{gsw} package is used.
#'
#' @inheritParams swRho
#' @return Sound speed [m/s].
#' @author Dan Kelley
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. \emph{Unesco Technical
#' Papers in Marine Science}, \bold{44}, 53 pp.  (See section 9.)
#' @examples
#' swSoundSpeed(40, T90fromT68(40), 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
#'
#' @family functions that calculate seawater properties
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
    ##np <- length(l$pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    l$pressure <- rep(l$pressure, length.out=nS)
    if (eos == "unesco") {
        res <- .C("sw_svel", as.integer(nS), as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                   value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_sound_speed(SA=SA, CT=CT, p=l$pressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}



#' Seawater specific heat
##' Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
##' check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
#'
#' Compute specific heat of seawater.
#'
#' If the first argument is a \code{ctd} object, then salinity, etc, are
#' extracted from it, and used for the calculation.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale.
#' @param pressure seawater pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} or \code{"gsw"}.
#' @return Specific heat \eqn{J kg^{-1}\,^\circ C^{-1}}{J/(kg degC)}
#' @author Dan Kelley
#' @references Millero et. al., J. Geophys. Res. 78 (1973), 4499-4507
#'
#' Millero et. al., UNESCO report 38 (1981), 99-188.
#' @examples
#' swSpecificHeat(40, T90fromT68(40), 10000, eos="unesco") # 3949.499
#' 
#' @family functions that calculate seawater properties
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
        res <- .Fortran("cp_driver", as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                         as.integer(nS), CP=double(nS))$CP
    } else {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        res <- gsw::gsw_cp_t_exact(SA=SA, t=l$temperature, p=l$pressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater spiciness
#'
#' Compute seawater "spice" (a variable orthogonal to density in TS space).
#'
#' If the first argument is a \code{ctd} object, then salinity, temperature and
#' pressure values are extracted from it, and used for the calculation.
#'
#' Roughly speaking, seawater with a high spiciness is relatively warm and
#' salty compared with less spicy water. Another interpretation is that spice
#' is a variable measuring distance orthogonal to isopycnal lines on TS
#' diagrams (if the diagrams are scaled to make the isopycnals run at 45
#' degres). The definition used here is that of Pierre Flament. (Other
#' formulations exist.)  Note that pressure is ignored in the definition.
#' Spiceness is sometimes denoted \eqn{\pi(S,t,p)}{pi(S,t,p)}.
#'
#' @param salinity either salinity [PSU] (in which case \code{temperature} and
#' \code{pressure} must be provided) \strong{or} a \code{ctd} object (in which
#' case \code{salinity}, \code{temperature} and \code{pressure} are determined
#' from the object, and must not be provided in the argument list).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C] on the
#' ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure seawater pressure [dbar]
#' @return Spice [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references P. Flament, 2002. A state variable for characterizing water
#' masses and their diffusive stability: spiciness.  \emph{Progr. Oceanog.},
#' \bold{54}, 493-501.
#' @family functions that calculate seawater properties
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
    res <- .C("sw_spice", as.integer(nS), as.double(l$salinity),
               as.double(T68fromT90(l$temperature)), as.double(l$pressure),
               value = double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater potential temperature
#'
#' Compute \eqn{\theta}{theta}, the potential temperature of seawater.
#'
#' The potential temperature is defined to be the temperature that a water
#' parcel of salinity \code{S}, \emph{in-situ} temperature \code{t} and
#' pressure \code{p} would have if were to be moved adiabatically to a location
#' with pressure \code{referencePressure}. This quantity is commonly denoted
#' \eqn{\theta}{theta} in the oceanographic literature.
#'
#' If the first argument is a \code{ctd} or \code{section} object, then values
#' for salinity, etc., are extracted from it, and used for the calculation, and
#' the corresponding arguments to the present function are ignored.
#'
#' For \code{eos="unesco"} the method of Fofonoff \emph{et al.} (1983), is used
#' [1,2].  For \code{eos="gsw"}, \code{\link[gsw]{gsw_pt_from_t}} is used
#' [3,4].
#'
#' @param salinity either salinity [PSU] (in which case \code{temperature} and
#' \code{pressure} must be provided) \strong{or} an \code{oce} object (in which
#' case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}, and the examples below.
#' @param pressure pressure [dbar]
#' @param referencePressure reference pressure [dbar]
#' @param longitude longitude of observation (only used if \code{eos="gsw"};
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if \code{eos="gsw"}; see
#' \sQuote{Details}).
#' @param eos equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
#' [3,4].
#' @return Potential temperature [\eqn{^\circ}{deg}C] of seawater.
#' @author Dan Kelley
#' @seealso The corresponding potential density anomaly
#' \eqn{\sigma_\theta}{sigma-theta} can be calculated with
#' \code{\link{swSigmaTheta}}.
#' @references
#'
#' [1] Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
#' Science}, \bold{44}, 53 pp
#'
#' [2] Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
#' York, 662 pp.
#'
#' [3] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' [4] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#' @examples
#' library(oce)
#' print(swTheta(40, T90fromT68(40), 10000, 0, eos="unesco")) # 36.89073 (Fofonoff et al., 1983)
#' 	
#' # Demonstrate that the UNESCO and GSW methods agree to a about 0.1C over a
#' # typical span of values.
#' S <- c(30,35,30,35)
#' T <- c(-2,-2,30,30)
#' p <- 1000 * runif(n=4)
#' print(max(abs(swTheta(S,T90fromT68(T),p) - swTheta(S,T,p,0,eos="gsw"))))
#'
#' # Example from a cross-Atlantic section
#' data(section)
#' stn <- section[['station', 70]]
#' plotProfile(stn, 'theta', ylim=c(6000, 1000))
#' lines(stn[['temperature']], stn[['pressure']], lty=2)
#' legend("topleft", lty=1:2,
#'        legend=c("potential", "in-situ"),
#'        bg='white', title="Station 70")
#'
#' @family functions that calculate seawater properties
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
    np <- length(l$pressure)
    if (np == 1)
        l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    referencePressure <- rep(referencePressure[1], length.out=nS)
    if (eos == "unesco") {
        res <- .C("theta_UNESCO_1983",
                   as.integer(nS), as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                   as.double(referencePressure),
                   value=double(nS), NAOK=TRUE, PACKAGE = "oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        res <- gsw::gsw_pt_from_t(SA=SA, t=l$temperature, p=l$pressure, p_ref=referencePressure)
    }
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater viscosity
#'
#' Compute viscosity of seawater, in \eqn{Pa\cdot s}{Pa*s}
#'
#' If the first argument is a \code{ctd} object, then salinity, temperature and
#' pressure values are extracted from it, and used for the calculation.
#'
#' The result is determined from a regression of the data provided in Table 87
#' of Dorsey (1940). The fit matches the table to within 0.2 percent at worst,
#' and with average absolute error of 0.07 percent. The maximum deviation from
#' the table is one unit in the last decimal place.
#'
#' No pressure dependence was reported by Dorsey (1940).
#'
#' @param salinity either salinity [PSU] (in which case \code{temperature} and
#' \code{pressure} must be provided) \strong{or} a \code{ctd} object (in which
#' case \code{salinity}, \code{temperature} and \code{pressure} are determined
#' from the object, and must not be provided in the argument list).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}, and the examples below.
#' @return Viscosity of seawater in \eqn{Pa\cdot s}{Pa*s}.  Divide by density
#' to get kinematic viscosity in \eqn{m^2/s}{m^2/s}.
#' @author Dan Kelley
#' @references N. Ernest Dorsey (1940), \emph{Properties of ordinary
#' Water-substance}, American Chemical Society Monograph Series.  Reinhold
#' Publishing.
#' @examples
#'
#' swViscosity(30, 10) # 0.001383779
#'
#' @family functions that calculate seawater properties
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


#' Seawater conservative temperature, in GSW formulation
#'
#' Compute seawater conservative temperature, according to the GSW/TEOS-10
#' formulation.
#'
#' If the first argument is an \code{oce} object, then values for salinity,
#' etc., are extracted from it, and used for the calculation, and the
#' corresponding arguments to the present function are ignored.
#'
#' The conservative temperature is calculated using the TEOS-10 function
#' \code{\link[gsw]{gsw_CT_from_t}} from the \code{gsw} package.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' @param pressure pressure [dbar]
#' @param longitude longitude of observation.
#' @param latitude latitude of observation.
#' @return Conservative temperature in degrees Celcius.
#' @author Dan Kelley
#' @seealso The related TEOS-10 quantity ``absolute salinity'' may be computed
#' with \code{\link{swAbsoluteSalinity}}.  For a ctd object, conservative
#' temperature may also be recovered by indexing as e.g.
#' \code{ctd[["conservativeTemperature"]]} or \code{ctd[["CT"]]}.
#' @references McDougall, T.J. and P.M. Barker, 2011: Getting started with
#' TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
#' SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.
#' @examples
#' swConservativeTemperature(35,10,1000,188,4) # 9.86883
#'
#' @family functions that calculate seawater properties
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
    SA <- gsw::gsw_SA_from_SP(SP=l$salinity[!bad], p=l$pressure[!bad],
                         longitude=l$longitude[!bad], latitude=l$latitude[!bad])
    good <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature[!bad], p=l$pressure[!bad])
    res <- rep(NA, nS)
    res[!bad] <- good
    if (Smatrix) dim(res) <- dim
    res
}


#' Seawater absolute salinity, in GSW formulation
#'
#' Compute seawater absolute salinity, according to the GSW/TEOS-10
#' formulation.
#'
#' The absolute salinity is calculated using the GSW function
#' \code{\link[gsw]{gsw_SA_from_SP}}.  Typically, this is a fraction of a unit
#' higher than practical salinity as defined in the UNESCO formulae.
#'
#' @param salinity either practical salinity (in which case \code{temperature}
#' and \code{pressure} must be provided) \strong{or} an \code{oce} object (in
#' which case \code{salinity}, etc. are inferred from the object).
#' @param pressure pressure in dbar.
#' @param longitude longitude of observation.
#' @param latitude latitude of observation.
#' @return Absolute Salinity in \eqn{g/kg}{g/kg}.
#' @author Dan Kelley
#' @seealso The related TEOS-10 quantity ``conservative temperature'' may be
#' computed with \code{\link{swConservativeTemperature}}.  For a ctd object,
#' absolute salinity may also be recovered by indexing as e.g.
#' \code{ctd[["absoluteSalinity"]]} or \code{ctd[["SA"]]}.
#' @references McDougall, T.J. and P.M. Barker, 2011: Getting started with
#' TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
#' SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.
#' @examples
#' \dontrun{
#' sa <- swAbsoluteSalinity(35.5, 300, 260, 16)
#' stopifnot(abs(35.671358392019094 - sa) < 00.000000000000010)
#' }
#'
#' @family functions that calculate seawater properties
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
    good <- gsw::gsw_SA_from_SP(l$salinity[!bad], l$pressure[!bad], l$longitude[!bad], l$latitude[!bad])
    res <- rep(NA, nS)
    res[!bad] <- good
    if (Smatrix) dim(res) <- dim
    res
}
