#' Convert from ITS-90 to IPTS-68 temperature
#'
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the ITS-90 scale.
#' @return Temperature expressed in the IPTS-68 scale.
T68fromT90 <- function(temperature) temperature * 1.00024

#' Convert from IPTS-68 to ITS-90 temperature
#'
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the IPTS-68 scale.
#' @return temperature Temperature expressed in the ITS-90 scale.
T90fromT68 <- function(temperature) temperature / 1.00024

#' Convert from ITS-48 to ITS-90 temperature
#'
#' @template temperatureConversionTemplate
#' @param temperature Vector of temperatures expressed in the ITS-48 scale.
#' @return Temperature expressed in the ITS-90 scale.
T90fromT48 <- function(temperature) (temperature-4.4e-6*temperature * (100-temperature))/1.00024

#' Look Within the First Element of a List for Replacement Values
#'
#' This is a helper function used by some seawater functions
#' (with names starting with `sw`) to
#' facilitate the specification of water properties either with
#' distinct arguments, or with data stored within an `oce`
#' object that is the first argument.
#'
#' If `list[1]` is not an `oce` object, then the
#' return value of `lookWithin` is the same as the input
#' value, except that (a) `eos` is completed to either
#' `"gsw"` or `"unesco"` and (b) if `longitude`
#' and `latitude` are within `list[1]`, then they
#' are possibly lengthened, to have the same length as the first
#' item in the `data` slot of `list[1]`.
#'
#' The examples may clarify this somewhat.
#'
#' @param list A list of elements, typically arguments that will be used in sw functions.
#'
#' @return A list with elements of the same names but possibly filled in from the first element.
#'
#' @examples
#' ## 1. If first item is not a CTD object, just return the input
#' lookWithin(list(a=1, b=2)) # returns a list
#' ## 2. Extract salinity from a CTD object
#' data(ctd)
#' str(lookWithin(list(salinity=ctd)))
#' ## 3. Extract salinity and temperature. Note that the
#' ## value specified for temperature is ignored; all that matters
#' ## is that temperature is named.
#' str(lookWithin(list(salinity=ctd, temperature=NULL)))
#' ## 4. How it is used by swRho()
#' rho1 <- swRho(ctd, eos="unesco")
#' rho2 <- swRho(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]], eos="unesco")
#' expect_equal(rho1, rho2)
lookWithin <- function(list)
{
    ##>>> message("in lookWithin")
    n <- length(list)
    names <- names(list)
    ## str(list)
    list1 <- list[[1]]
    if (inherits(list[[1]], "oce")) {
        ##>>> message("  in lookWithin, list type; n=", n)
        for (i in 1:n) {
            ##>>> message("names[", i, "]: ", names[i])
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
            ##>>> message(" lookWithin it is a CTD")
            nrows <- length(list[[names[1]]])
            if (length(list[["longitude"]]))
                list[["longitude"]] <- rep(mean(list[["longitude"]], na.rm=TRUE), nrows)
            if (length(list[["latitude"]]))
                list[["latitude"]] <- rep(mean(list[["latitude"]], na.rm=TRUE), nrows)
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
#' This computes Rrho (density ratio) from a `ctd` object.
#'
#' If `eos="unesco"`, this is done by calculating salinity and
#' potential-temperature derivatives from smoothing splines whose properties
#' are governed by `smoothingLength` or `df`.  If
#' `sense="diffusive"` the definition is
#' \eqn{(beta*dS/dz)/(alpha*d(theta)/dz)}{(beta*dS/dz)/(alpha*d(theta)/dz)} and
#' the reciprocal for `"finger"`.
#'
#' If `eos="gsw"`, this is done by extracting absolute salinity and
#' conservative temperature, smoothing with a smoothing spline as in the
#' `"unesco"` case, and then calling [gsw::gsw_Turner_Rsubrho()]
#' on these smoothed fields. Since the gsw function works on mid-point
#' pressures, [approx()] is used to interpolate back to the original
#' pressures.
#'
#' If the default arguments are acceptable, `ctd[["Rrho"]]` may be used
#' instead of `swRrho(ctd)`.
#'
#' @param ctd an object of class `ctd`
#' @param sense an indication of the sense of double diffusion under study and
#' therefore of the definition of Rrho; see \sQuote{Details}
#' @param smoothingLength ignored if `df` supplied, but otherwise the
#' latter is calculated as the number of data points, divided by the number
#' within a depth interval of `smoothingLength` metres.
#' @param df if given, this is provided to [smooth.spline()].
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#' @return Density ratio defined in either the `"diffusive"` or
#' `"finger"` sense.
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
#' may optionally be done with [smooth.spline()], unless
#' `df=NA`, in which case raw data are used.  If `df` is not
#' provided, a possibly reasonable value computed from an analysis of the
#' profile, based on the number of pressure levels.
#'
#' The core of the
#' method involves differentiating potential density (referenced to median
#' pressure) with respect to pressure, and the `derivs` argument is used
#' to control how this is done, as follows.
#'
#' * If `derivs` is not supplied, the action is as though it were
#' given as the string `"smoothing"`
#'
#' * If `derivs` equals `"simple"`, then the derivative of
#' density with respect to pressure is calculated as the ratio of first-order
#' derivatives of density and pressure, each calculated using
#' [diff()].  (A zero is appended at the top level.)
#'
#' * If `derivs` equals `"smoothing"`, then the processing
#' depends on the number of data in the profile, and on whether `df` is
#' given as an optional argument.  When the number of points exceeds 4, and
#' when `df` exceeds 1, [smooth.spline()] is used to calculate
#' smoothing spline representation the variation of density as a function of
#' pressure, and derivatives are extracted from the spline using
#' `predict`.  Otherwise, density is smoothed using [smooth()],
#' and derivatives are calculated as with the `"simple"` method.
#'
#' * If `derivs` is a function taking two arguments (first pressure,
#' then density) then that function is called directly to calculate the
#' derivative, and no smoothing is done before or after that call.
#'
#' For precise work, it makes sense to skip `swN2` entirely, choosing
#' whether, what, and how to smooth based on an understanding of fundamental
#' principles as well as data practicalities.
#'
#' @param pressure either pressure (dbar) (in which case `sigmaTheta` must
#' be provided) *or* an object of class `ctd` object (in which case
#' `sigmaTheta` is inferred from the object.
#'
#' @param sigmaTheta Surface-referenced potential density minus 1000
#' (kg/m\eqn{^3}{^3}).
#'
#' @param derivs optional argument to control how the derivative
#' \eqn{d\sigma_\theta/dp}{d(sigmaTheta)/d(pressure)} is calculated.  This may
#' be a character string or a function of two arguments.  See \dQuote{Details}.
#'
#' @param df argument passed to [smooth.spline()] if this function is
#' used for smoothing; set to `NA` to prevent smoothing.
#'
#' @param \dots additional argument, passed to [smooth.spline()], in
#' the case that `derivs="smoothing"`.  See \dQuote{Details}.
#' @template debugTemplate
#'
#' @seealso The [gsw::gsw_Nsquared()] function of the \CRANpkg{gsw}
#' provides an alternative to this, as formulated in the GSW system. It
#' has a more sophisticated treatment of potential density, but it is based
#' on simple first-difference derivatives, so its results may require
#' smoothing, depending on the dataset and purpose of the analysis.
#'
#' @return Square of buoyancy frequency (\eqn{radian^2/s^2}{radian^2/s^2}).
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' data(ctd)
#' # Left panel: density
#' p <- ctd[["pressure"]]
#' ylim <- rev(range(p))
#' par(mfrow=c(1, 2), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(ctd[["sigmaTheta"]], p, ylim=ylim, type='l', xlab=expression(sigma[theta]))
#' # Right panel: N2, with default settings (black) and with df=2 (red)
#' N2 <- swN2(ctd)
#' plot(N2, p, ylim=ylim, xlab="N2 [1/s^2]", ylab="p", type="l")
#' lines(swN2(ctd, df=3), p, col=2)
#'
#' @section Deprecation Notice:
#' Until 2019 April 11, `swN2` had an argument named `eos`. However,
#' this did not work as stated, unless the first argument was a `ctd`
#' object. Besides, the argument name was inherently deceptive, because the UNESCO
#' scheme does not specify how N2 is to be calculated.
#' Nothing is really lost by making this change, because the new default is the
#' same as was previously available with the `eos="unesco"`
#' setup, and the gsw-formulated estimate of N2 is provided
#' by [gsw::gsw_Nsquared()] in the \CRANpkg{gsw} package.
#'
#' @family functions that calculate seawater properties
swN2 <- function(pressure, sigmaTheta=NULL, derivs, df,
                 debug=getOption("oceDebug"),  ...)
{
    oceDebug(debug, "swN2(...) {\n", sep="", unindent=1)
    ##cat("swN2(..., df=", df, ")\n",sep="")
    ##useSmoothing <- !missing(df) && is.finite(df)
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
                sigmaThetaDeriv[ok] <- predict(sigmaThetaSmooth, pressure[ok], deriv=1)$y
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
    oceDebug(debug, "} # swN2()\n", sep="", unindent=1)
    res
}


#' Water pressure
#'
#' Compute seawater pressure from depth by inverting [swDepth()]
#' using [uniroot()].
#'
#' If `eos="unesco"` this is done by numerical inversion of
#' [swDepth()] is done using [uniroot()]. If
#' `eos="gsw"`, it is done using [gsw::gsw_p_from_z()] in the
#' \CRANpkg{gsw} package.
#'
#' @param depth distance below the surface in metres.
#'
#' @param latitude Latitude in \eqn{^\circ}{deg}N or radians north of the
#' equator.
#'
#' @param eos indication of formulation to be used, either `"unesco"` or
#' `"gsw"`.
#'
#' @return Pressure in dbar.
#'
#' @author Dan Kelley
#'
#' @references Unesco 1983. Algorithms for computation of fundamental
#' properties of seawater, 1983. *Unesco Tech. Pap. in Mar. Sci.*, No. 44,
#' 53 pp.
#'
#' @examples
#' swPressure(9712.653, 30, eos="unesco") # 10000
#' swPressure(9712.653, 30, eos="gsw")    #  9998.863
#'
#' @family functions that calculate seawater properties
swPressure <- function(depth, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(depth))
        stop("must supply depth")
    ## FIXME-gsw add gsw version
    ndepth <- length(depth)
    if (length(latitude) < ndepth)
        latitude <- rep(latitude, ndepth)
    res <- vector("numeric", ndepth)
    eos <- match.arg(eos, c("unesco", "gsw"))
    ## Takes 3.55s for 15225 points
    if (eos == "unesco") {
        for (i in 1:ndepth) {
            ## FIXME: this loop is slow and should be done in C, like swCStp()
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
#' If `eos="unesco"`, the calculation is done by a bisection root search
#' on the UNESCO formula relating salinity to conductivity, temperature, and
#' pressure (see [swSCTp()]).  If it is `"gsw"` then the
#' Gibbs-SeaWater formulation is used, via [gsw_C_from_SP()].
#'
#' @param salinity practical salinity, or a CTD object (in which case its
#' temperature and pressure are used, and the next two arguments are ignored)
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see the examples, as well as the
#' \dQuote{Temperature units} section in the documentation for [swRho()].
#'
#' @param pressure pressure (dbar)
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#'
#' @return Conductivity ratio (unitless), i.e. the ratio of conductivity to the
#' conductivity at salinity=35, temperature=15 (IPTS-68 scale) and pressure=0,
#' which has numerical value 42.9140 mS/cm = 4.29140 S/m (see
#' Culkin and Smith, 1980, in the regression result cited at the bottom of
#' the left-hand column on page 23).
#'
#' @author Dan Kelley
#'
#' @seealso For thermal (as opposed to electrical) conductivity, see
#' [swThermalConductivity()]. For computation of salinity from
#' electrical conductivity, see [swSCTp()].
#'
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. *Unesco Technical
#' Papers in Marine Science*, *44*, 53 pp.
#'
#' 2. Culkin, F., and Norman D. Smith, 1980. Determination of the concentration of
#' potassium chloride solution having the same electrical conductivity, at 15 C
#' and infinite frequency, as standard seawater of salinity 35.0000 ppt
#' (Chlorinity 19.37394 ppt). *IEEE Journal of Oceanic Engineering*,
#' *5*, pp 22-23.
#'
#' @examples
#' expect_equal(1, swCSTp(35, T90fromT68(15), 0, eos="unesco")) # by definition of cond. ratio
#' expect_equal(1, swCSTp(34.25045, T90fromT68(15), 2000, eos="unesco"), tolerance=1e-7)
#' expect_equal(1, swCSTp(34.25045, T90fromT68(15), 2000, eos="gsw"), tolerance=1e-7)
#'
#' @family functions that calculate seawater properties
swCSTp <- function(salinity, temperature=15, pressure=0,
                   eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        ctd <- salinity
        salinity <- ctd[["salinity"]]
        temperature <- ctd[["temperature"]]
        pressure <- ctd[["pressure"]]
    }
    dim <- dim(salinity)
    salinity <- as.vector(salinity)
    temperature <- as.vector(temperature)
    pressure <- as.vector(pressure)
    n <- length(salinity)
    if (length(temperature) != n)
        temperature <- rep(temperature, length.out=n)
    if (length(pressure) != n)
        pressure <- rep(pressure, length.out=n)
    eos <- match.arg(eos, c("unesco", "gsw"))
    if (eos == "unesco") {
        ## cat("S= ", paste(salinity, collapse=" "), "\n")
        ## cat("T= ", paste(temperature, collapse=" "), "\n")
        ## cat("p= ", paste(pressure, collapse=" "), "\n")
        res <- .C("sw_CSTp",
                  as.integer(n), as.double(salinity), as.double(T68fromT90(temperature)), as.double(pressure),
                  C=double(n), NAOK=TRUE, PACKAGE="oce")$C
    } else {
        ## for the use of a constant, as opposed to a function call with (35,15,0), see
        ## https://github.com/dankelley/oce/issues/746
        res <- gsw::gsw_C_from_SP(SP=salinity, t=temperature, p=pressure) / 42.9140
    }
    dim(res) <- dim
    res
}


#' Practical salinity from electrical conductivity, temperature and pressure
#'
#' Calculate salinity from what is actually measured by a CTD, *i.e.*
#' conductivity, *in-situ* temperature and pressure.  Often this is done
#' by the CTD processing software, but sometimes it is helpful to do this
#' directly, *e.g.* when there is a concern about mismatches in sensor
#' response times.  Two variants are provided. First, if `eos` is
#' `"unesco"`, then salinity is calculated using
#' the UNESCO algorithm described by Fofonoff and Millard (1983) as in
#' reference 1. Second, if `eos` is `"gsw"`, then the
#' Gibbs-SeaWater formulation is used, via [gsw::gsw_SP_from_C()]
#' in the \CRANpkg{gsw} package. The latter starts with the same formula
#' as the former, but if this yields a Practical Salinity less than 2,
#' then the result is instead calculated using
#' formulae provided by Hill et al. (1986; reference 2), modified to match the
#' `"unesco"` value at Practical salinity equal to 2 (reference 3).
#'
#' @param conductivity a measure of conductivity (see also `conductivityUnit`)
#' or an `oce` object holding hydrographic information. In the second case,
#' all the other arguments to `swSCTp` are ignored.
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure pressure (dbar).
#'
#' @param conductivityUnit string indicating the unit used for conductivity.
#' This may be `"ratio"` or `""` (meaning conductivity ratio),
#' `"mS/cm"` or `"S/m"`.  Note that the ratio mode assumes that
#' measured conductivity has been divided by the standard conductivity
#' of 4.2914 S/m.
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.#'
#' @return Practical Salinity.
#'
#' @author Dan Kelley
#'
#' @seealso For thermal (as opposed to electrical) conductivity, see
#' [swThermalConductivity()].  For computation of electrical
#' conductivity from salinity, see [swCSTp()].
#'
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. *Unesco Technical
#' Papers in Marine Science*, *44*, 53 pp.
#'
#' 2.  K. Hill, T. Dauphinee, and D. Woods. \dQuote{The Extension of the Practical
#' Salinity Scale 1978 to Low Salinities.} IEEE Journal of Oceanic Engineering 11,
#' no. 1 (January 1986): 109-12. https://doi.org/10.1109/JOE.1986.1145154.
#'
#' 3. `gsw_from_SP` online documentation, available at
#' `http://www.teos-10.org/pubs/gsw/html/gsw_C_from_SP.html`
#'
#' @examples
#' # 1. Demonstrate agreement with test value in UNESCO documents
#' swSCTp(1, T90fromT68(15), 0, eos="unesco") # expect 35
#' # 2. Demonstrate agreement of gsw and unesco, S>2 case
#' swSCTp(1, T90fromT68(15), 0, eos="gsw") # again, expect 35
#' # 3. Demonstrate close values even in very brackish water
#' swSCTp(0.02, 10, 100, eos="gsw")    # 0.6013981
#' swSCTp(0.02, 10, 100, eos="unesco") # 0.6011721
#'
#' @family functions that calculate seawater properties
swSCTp <- function(conductivity, temperature=NULL, pressure=NULL,
                   conductivityUnit, eos=getOption("oceEOS", default="gsw"))
{
    C0 <- 42.9140 # Culkin and Smith (1980)
    ## FIXME-gsw add gsw version
    if (missing(conductivity))
        stop("must supply conductivity (which may be S or a CTD object)")
    if (missing(conductivityUnit)) {
        conductivityUnit <- ""
    } else {
        if (is.list(conductivityUnit) && "unit" %in% names(conductivityUnit))
            conductivityUnit <- conductivityUnit$unit
        if (is.expression(conductivityUnit))
            conductivityUnit <- as.character(conductivityUnit)
        if (conductivityUnit == "ratio")
            conductivityUnit <- ""
    }
    if (conductivityUnit != "" && conductivityUnit != "mS/cm" && conductivityUnit != "S/m")
        stop("conductivity unit must be \"\", \"mS/cm\", or \"S/m\"")
    if (inherits(conductivity, "oce")) {
        if (inherits(conductivity, "rsk")) {
            ctd <- as.ctd(conductivity)
        } else {
            ctd <- conductivity
        }
        ## cat("< ", paste(names(ctd@data), collapse=" "), " >\n", sep="")
        conductivity <- ctd[["conductivity"]]
        if (is.null(conductivity))
            stop("this CTD object has no conductivity")
        ## Use unit from within the object, but may be overridden after this block.
        tmp <- ctd[["conductivityUnit"]]
        if (is.list(tmp) && "unit" %in% names(tmp))
            conductivityUnit <- as.character(tmp$unit)
        temperature <- ctd[["temperature"]]
        pressure <- ctd[["pressure"]]
    }
    if (is.list(conductivityUnit)) {
        conductivityUnit <- as.character(conductivityUnit$unit)
    }
    if (!length(conductivityUnit))
        conductivityUnit <- ""
    if (conductivityUnit == "mS/cm")
        conductivity <- conductivity / C0
    else if (conductivityUnit == "S/m")
        conductivity <- conductivity / (C0 / 10)
    else
        conductivity <- conductivity
    ## Now, "conductivity" is in ratio form
    dim <- dim(conductivity)
    nC <- length(conductivity)
    nT <- length(temperature)
    if (nC != nT)
        stop("lengths of conductivity and temperature must agree, but they are ", nC, " and ", nT)
    if (is.null(pressure))
        pressure <- rep(0, nC)
    np <- length(pressure)
    if (nC != np)
        stop("lengths of conductivity and pressure must agree, but they are ", nC, " and ", np)
    if (eos == "unesco") {
        ##> message("swSCTp() unesco; conductivity[1]=", conductivity[1], ", temperature[1]=", temperature[1], ", pressure[1]=", pressure[1])
        res <- .C("sw_salinity",
                   as.integer(nC),
                   as.double(conductivity),
                   as.double(T68fromT90(temperature)), # original formula is in IPTS-68 but we now use ITS-90
                   as.double(pressure),
                   value=double(nC),
                   NAOK=TRUE, PACKAGE="oce")$value
    } else if (eos == "gsw") {
        ## we don't need to convert to IPTS-68 for the gsw formulation, because it is already formulated
        ## to work with ITS-90
        ##> message("swSCTp() gsw; conductivity[1]=", conductivity[1], ", temperature[1]=", temperature[1], ", pressure[1]=", pressure[1])
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
#' depending on the value of `eos`.
#'
#' For `eos="unesco"`, finds the practical salinity that yields the given
#' density, with the given in-situ temperature and pressure.  The method is a
#' bisection search with a salinity tolerance of 0.001.  For `eos="gsw"`,
#' the function [gsw::gsw_SA_from_rho()] in the \CRANpkg{gsw}
#' package is used
#' to infer Absolute Salinity from Conservative Temperature.
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param density *in-situ* density or sigma value (\eqn{kg/m^3}{kg/m^3})
#'
#' @param pressure *in-situ* pressure (dbar)
#'
#' @param eos equation of state, either `"unesco"` (see references 1 and 2)
#' or `"gsw"` (see references 3 and 4).
#'
#' @return Practical Salinity, if `eos="unesco"`, or Absolute Salinity, if
#' `eos="gsw"`.
#'
#' @author Dan Kelley
#'
#' @seealso [swTSrho()]
#'
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater. *Unesco Technical Papers in Marine
#' Science*, *44*, 53 pp
#'
#' 2. Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
#' York, 662 pp.
#'
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 4. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#'
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
#' Compute *in-situ* temperature, given salinity, density, and pressure.
#'
#' Finds the temperature that yields the given density, with the given salinity
#' and pressure.  The method is a bisection search with temperature tolerance
#' 0.001 \eqn{^\circ C}{degC}.
#'
#' @param salinity *in-situ* salinity (PSU)
#'
#' @param density *in-situ* density or sigma value (kg/m\eqn{^3}{^3})
#'
#' @param pressure *in-situ* pressure (dbar)
#'
#' @param eos equation of state to be used, either `"unesco"` or
#' `"gsw"` (ignored at present).
#'
#' @return *In-situ* temperature (\eqn{^\circ C}{degC}) in the ITS-90
#' scale.
#'
#' @author Dan Kelley
#'
#' @seealso [swSTrho()]
#'
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater. *Unesco Technical
#' Papers in Marine Science*, *44*, 53 pp
#'
#' Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
#' York, 662 pp.
#'
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
    for (i in 1:nS) {
        ## FIXME: avoid loops
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
                     temperature=double(1),
                     NAOK=TRUE, PACKAGE="oce")$t
        this.T <- T90fromT68(this.T)
        if (i == 1) res <- this.T else res <- c(res, this.T)
    }
    dim(res) <- dim
    res
}


#' Seawater freezing temperature
#'
#' Compute in-situ freezing temperature of seawater, using either the UNESCO formulation
#' (computed as in Section 5 of reference 1) or the GSW formulation (computed
#' by using [gsw::gsw_SA_from_SP()] to get Absolute Salinity, and
#' then [gsw::gsw_t_freezing()] to get the freezing temperature).
#'
#' If the first argument is an `oce` object, and if the `pressure`
#' argument is `NULL`, then the pressure is sought within the first
#' argument. In the case of `eos="gsw"`, then a similar procedure
#' also applies to the `longitude` and `latitude` arguments.
#'
#' @param salinity Either practical salinity (PSU) or a `ctd` object from which
#' practical salinity and pressure (plus in the `eos="gsw"` case,
#' longitude and latitude) are inferred, using [lookWithin()].
#'
#' @param pressure Seawater pressure (dbar).
#'
#' @param longitude Longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude Latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param saturation_fraction The saturation fraction of dissolved air in seawater,
#' ignored if `eos="unesco"`).
#'
#' @param eos The equation of state, either `"unesco"` (references 1 and 2) or `"gsw"`.
#' (references 3 and 4).
#'
#' @return Temperature (\eqn{^\circ}{deg}C), defined on the ITS-90 scale.
#'
#' @author Dan Kelley
#'
#' @references
#' 1. Fofonoff, N. P., and R. C. Millard. \dQuote{Algorithms for Computation of
#' Fundamental Properties of Seawater.} UNESCO Technical Papers in Marine
#' Research. SCOR working group on Evaluation of CTD data; UNESCO/ICES/SCOR/IAPSO
#' Joint Panel on Oceanographic Tables and Standards, 1983.
#' https://unesdoc.unesco.org/ark:/48223/pf0000059832.
#'
#' 2. Gill, A E. Atmosphere-Ocean Dynamics. New York, NY, USA: Academic Press,
#' 1982.
#'
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide, 2010.
#'
#' 4. McDougall, Trevor J., and Paul M. Barker. Getting Started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox. SCOR/IAPSO WG127, 2011.
#'
#' @examples
#' # 1. Test for a check-value given in reference 1. This value, -2.588567 degC,
#' # is in the 1968 temperature scale (IPTS-68), but swTFreeze reports
#' # in the newer ITS-90 scale, so we must convert before checking.
#' Tcheck <- -2.588567 # IPTS-68
#' T <- swTFreeze(salinity=40, pressure=500, eos="unesco")
#' expect_equal(Tcheck, T68fromT90(T), tolerance=1e-6)
#'
#' # 2. Compare unesco and gsw formulations.
#' data(ctd)
#' p <- ctd[["pressure"]]
#' par(mfrow=c(1, 2), mar=c(3, 3, 1, 2), mgp=c(2, 0.7, 0))
#' plot(swTFreeze(ctd, eos="unesco"),
#'      p, xlab="unesco", ylim=rev(range(p)))
#' plot(swTFreeze(ctd, eos="unesco") - swTFreeze(ctd, eos="gsw"),
#'      p, xlab="unesco-gsw", ylim=rev(range(p)))
#'
#' @family functions that calculate seawater properties
swTFreeze <- function(salinity, pressure=NULL,
                      longitude=NULL, latitude=NULL, saturation_fraction=1,
                      eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must supply salinity (which may be S or a CTD object)")
    if (inherits(salinity, "oce")) {
        if (is.null(pressure))
            pressure <- salinity[["pressure"]]
    }
    if (is.null(pressure))
        stop("must supply pressure")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        ## Note: the pressure in the next line is for computing SA; see below.
        l <- lookWithin(list(salinity=salinity, latitude=latitude, longitude=longitude, pressure=pressure))
    } else {
        l <- lookWithin(list(salinity=salinity, pressure=pressure))
    }
    dim <- dim(l$salinity)
    if (eos == "gsw") {
        ## Note that l$pressure is used for computing SA, but not for gsw_t_freezing().
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        res <- gsw::gsw_t_freezing(SA=SA, p=l$pressure, saturation_fraction=saturation_fraction)
    } else if (eos == "unesco") {
        res <- (-.0575+1.710523e-3*sqrt(abs(l$salinity))-2.154996e-4*l$salinity)*l$salinity-7.53e-4*l$pressure
        res <- T90fromT68(res)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater thermal expansion coefficient
#'
#' Compute \eqn{\alpha}{alpha}, the thermal expansion coefficient for seawater.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#'
#' @return Value in 1/degC.
#'
#' @author Dan Kelley
#'
#' @references The `eos="unesco"` formulae are based on the UNESCO
#' equation of state, but are formulated empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The `eos="gsw"` formulae come from GSW; see references in
#' the [swRho()] documentation.
#'
#' @family functions that calculate seawater properties
swAlpha <- function(salinity, temperature=NULL, pressure=0,
                    longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
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
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C)
#'
#' @param pressure pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#'
#' @return Value in psu/\eqn{^\circ}{deg}C.
#'
#' @author Dan Kelley
#'
#' @references The `eos="unesco"` formulae are based on the UNESCO
#' equation of state, but are formulated empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The `eos="gsw"` formulae come from GSW; see references in
#' the [swRho()] documentation.
#'
#' @examples
#' swAlphaOverBeta(40, 10, 4000, eos="unesco") # 0.3476
#'
#' @family functions that calculate seawater properties
swAlphaOverBeta <- function(salinity, temperature=NULL, pressure=NULL,
                   longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            temperature <- salinity[["temperature"]]
            pressure <- salinity[["pressure"]]
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    dim <- dim(l$salinity)
    if (is.null(l$temperature))
        stop("must provide temperature")
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(l$pressure)) pressure <- 0
    if (length(l$pressure) != nS) l$pressure <- rep(l$pressure, length.out=nS)
    if (l$eos == "gsw") {
        ## not likely to be called since gsw has a direct function for alpha, but put this here anyway
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_alpha_on_beta(SA=SA, CT=CT, p=l$pressure)
    } else if (l$eos == "unesco") {
        theta <- swTheta(l$salinity, l$temperature, l$pressure, eos="unesco")
        res <- .C("sw_alpha_over_beta", as.integer(nS),
                   as.double(l$salinity), as.double(theta), as.double(l$pressure),
                   value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater haline contraction coefficient
#'
#' Compute \eqn{\beta}{beta}, the haline contraction coefficient for seawater.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure seawater pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#'
#' @return Value in 1/psu.
#'
#' @author Dan Kelley
#'
#' @references The `eos="unesco"` formulae are based on the UNESCO
#' equation of state, but are formulated empirically by Trevor J. McDougall,
#' 1987, Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
#' 1950-1964. The `eos="gsw"` formulae come from GSW; see references in
#' the [swRho()] documentation.
#'
#' @family functions that calculate seawater properties
swBeta <- function(salinity, temperature=NULL, pressure=0,
                   longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        temperature <- salinity[["temperature"]]
        pressure <- salinity[["pressure"]]
        if (is.null(longitude))
            longitude <- salinity[["longitude"]]
        if (is.null(latitude))
            latitude <- salinity[["latitude"]]
    }
    if (eos == "gsw") {
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (length(l$pressure) == 1) l$pressure <- rep(l$pressure, length.out=nS)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_beta(SA=SA, CT=CT, p=l$pressure)
    } else if (eos == "unesco") {
        theta <- swTheta(l$salinity, l$temperature, l$pressure, eos="unesco") # the formula is i.t.o. theta
        res <- .C("sw_beta", as.integer(nS),
                   as.double(l$salinity), as.double(theta), as.double(l$pressure),
                   value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    }
    if (!is.null(dim))
        dim(res) <- dim
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
#' @param salinity salinity (PSU), or a `ctd` object, in which case
#' `temperature` and `pressure` will be ignored.
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure pressure (dbar)
#'
#' @return Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.
#' To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide by the
#' product of density and specific heat, as in the example.
#'
#' @author Dan Kelley
#'
#' @references Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
#' *Deep-sea Research*, *21*, 131-137.
#'
#' @examples
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
    if (missing(salinity))
        stop("must provide salinity")
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
        cond <- 0.001365 * (1+0.003*T - 1.025e-5*T^2 + 0.0653 * p - 0.00029 * S)
    }
    418.400 * cond                     # convert from cal/(cm*sec*degC) to J/(m*sec*degC)
}


#' Water depth
#'
#' Compute depth below the surface (i.e. a positive number within the
#' water column) based on pressure and latitude. (Use [swZ()]
#' to get the vertical coordinate, which is negative within the water
#' column.)
#'
#' If `eos="unesco"` then depth is calculated from pressure using Saunders
#' and Fofonoff's method, with the formula refitted for 1980 UNESCO equation of
#' state (reference 1).  If `eos="gsw"`, then [gsw::gsw_z_from_p()] from
#' the \CRANpkg{gsw} package (references 2 and 3) is used.
#'
#' @param pressure either pressure (dbar), in which case `lat` must also
#' be given, or a `ctd` object, in which case `lat` will be inferred
#' from the object.
#'
#' @param latitude Latitude in \eqn{^\circ}{deg}N or radians north of the
#' equator.
#'
#' @param eos indication of formulation to be used, either `"unesco"` or
#' `"gsw"`.
#'
#' @return Depth below the ocean surface, in metres.
#'
#' @author Dan Kelley
#'
#' @references
#' 1. Unesco 1983. Algorithms for computation of fundamental
#' properties of seawater, 1983. *Unesco Tech. Pap. in Mar. Sci.*, No. 44,
#' 53 pp.
#'
#' 2. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 3. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#'
#' @examples
#' d <- swDepth(10, 45)
#'
#' @family functions that calculate seawater properties
swDepth <- function(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure))
        stop("must provide pressure")
    l <- lookWithin(list(pressure=pressure, latitude=latitude, eos=eos))
    if (any(is.na(l$latitude)))
        l$latitude <- 45 # default to mid latitudes
    if (l$eos == "unesco") {
        l$latitude <- l$latitude * atan2(1, 1) / 45
        x <- sin(l$latitude)^2
        gr <- 9.780318 * (1.0 + (5.2788e-3+2.36e-5*x)*x) + 1.092e-6*l$pressure
        res <- ( ( (-1.82e-15*l$pressure+2.279e-10)*l$pressure-2.2512e-5)*l$pressure+9.72659)*l$pressure / gr
    } else if (l$eos == "gsw") {
        res <- -gsw::gsw_z_from_p(p=l$pressure, latitude=l$latitude)
    }
    res
}

#' Vertical coordinate
#'
#' Compute height above the surface. This is the negative of depth,
#' and so is defined simply in terms of [swDepth()].
#'
#' @inheritParams swDepth
#'
#' @family functions that calculate seawater properties
swZ <- function(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
{
    ## FIXME-gsw need a gsw version but it is not in the C library as of Dec 2014
    if (missing(pressure))
        stop("must provide pressure")
    -swDepth(pressure=pressure, latitude=latitude, eos=eos)
}


#' Dynamic height of seawater profile
#'
#' Compute the dynamic height of a column of seawater.
#'
#' If the first argument is a `section`, then dynamic height is calculated
#' for each station within a section, and returns a list containing distance
#' along the section along with dynamic height.
#'
#' If the first argument is a `ctd`, then this returns just a single
#' value, the dynamic height.
#'
#' If `eos="unesco"`, processing is as follows.  First, a piecewise-linear
#' model of the density variation with pressure is developed using
#' [stats::approxfun()]. (The option `rule=2` is used to
#' extrapolate the uppermost density up to the surface, preventing a possible a
#' bias for bottle data, in which the first depth may be a few metres below the
#' surface.) A second function is constructed as the density of water with
#' salinity 35PSU, temperature of 0\eqn{^\circ}{deg}C, and pressure as in the
#' `ctd`. The difference of the reciprocals of these densities, is then
#' integrated with [stats::integrate()] with pressure limits `0`
#' to `referencePressure`.  (For improved numerical results, the variables
#' are scaled before the integration, making both independent and dependent
#' variables be of order one.)
#'
#' If `eos="gsw"`, [gsw::gsw_geo_strf_dyn_height()] is used
#' to calculate a result in m^2/s^2, and this is divided by
#' 9.7963\eqn{m/s^2}{m/s^2}.
#' If pressures are out of order, the data are sorted. If any pressure
#' is repeated, only the first level is used.
#' If there are under 4 remaining distinct
#' pressures, `NA` is returned, with a warning.
#'
#' @param x a [section-class] object.
#'
#' @param referencePressure reference pressure (dbar). If this exceeds the
#' highest pressure supplied to [swDynamicHeight()], then that highest
#' pressure is used, instead of the supplied value of
#' `referencePressure`.
#'
#' @param subdivisions number of subdivisions for call to
#' [integrate()].  (The default value is considerably larger than the
#' default for [integrate()], because otherwise some test profiles
#' failed to integrate.
#'
#' @param rel.tol absolute tolerance for call to [integrate()].  Note
#' that this call is made in scaled coordinates, i.e. pressure is divided by
#' its maximum value, and dz/dp is also divided by its maximum.
#'
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
#'
#' @return In the first form, a list containing `distance`, the distance
#' (km( from the first station in the section and `height`, the dynamic
#' height (m). In the second form, a single value, containing the
#' dynamic height (m).
#'
#' @author Dan Kelley
#'
#' @references Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic
#' Press, New York, 662 pp.
#'
#' @examples
#'\dontrun{
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
#' plot(smu$x[-1], v, type='l', col="blue", xlab="distance \[km\]", ylab="velocity (m/s)")
#'
#' # right-hand column: gulf stream region, unsmoothed
#' gs <- subset(section, 102<=stationId&stationId<=124)
#' dh.gs <- swDynamicHeight(gs)
#' plot(dh.gs$distance, dh.gs$height, type='b', xlab="", ylab="dyn. height [m]")
#' v <- diff(dh.gs$height)/diff(dh.gs$distance) * g / f / 1e3
#' plot(dh.gs$distance[-1], v, type='l', col="blue",
#'   xlab="distance [km]", ylab="velocity (m/s)")
#'}
#'
#' @family functions that calculate seawater properties
swDynamicHeight <- function(x, referencePressure=2000,
                            subdivisions=500, rel.tol=.Machine$double.eps^0.25,
                            eos=getOption("oceEOS", default="gsw"))
{
    eos <- match.arg(eos, c("unesco", "gsw"))
    height <- function(ctd, referencePressure, subdivisions, rel.tol, eos=getOption("oceEOS", default="gsw"))
    {
        if (sum(!is.na(ctd@data$pressure)) < 2)
            return(NA)
        g <- if (is.na(ctd@metadata$latitude)) 9.8 else gravity(ctd@metadata$latitude)
        p <- ctd[["pressure"]]
        np <- length(p)
        p_ref <- min(max(p, na.rm=TRUE), referencePressure)
        if (eos == "unesco") {
            rho <- swRho(ctd, eos=eos)
            if (sum(!is.na(rho)) < 2)
                return(NA)
            ## 1e4 converts decibar to Pa
            dzdp <- ( (1/rho - 1/swRho(rep(35, np), rep(0, np), p, eos=eos)) / g )*1e4
            ## Scale both pressure and dz/dp to make integration work better (issue 499)
            max <- max(dzdp, na.rm=TRUE)
            integrand <- approxfun(p/p_ref, dzdp/max, rule=2)
            ##plot(dzdp/max, ctd@data$pressure/referencePressure, type='l')
            res <- integrate(integrand, 0, 1,
                             subdivisions=subdivisions, rel.tol=rel.tol)$value * p_ref * max
        } else {                       # "gsw"
            if (np > 3) {
                o <- order(p)
                p <- p[o]
                SA <- ctd[["SA"]][o]
                CT <- ctd[["CT"]][o]
                ## handle repeated values
                dp0 <- diff(p) == 0
                if (any(dp0)) {
                    SA <- SA[!dp0]
                    CT <- CT[!dp0]
                    p <- p[!dp0]
                    g <- 9.7963 # NOTE: do not use local gravity; the gsw is defined to use this value
                    if (length(p) < 4) {
                        warning("In swDynamicHeight() : returning NA since < 4 levels", call.=FALSE)
                        res <- NA
                    } else {
                        res <- gsw::gsw_geo_strf_dyn_height(SA=SA, CT=CT, p=p, p_ref=p_ref)[1] / g
                    }
                } else {
                    res <- gsw::gsw_geo_strf_dyn_height(SA=SA, CT=CT, p=p, p_ref=p_ref)[1] / g
                }
                res[is.nan(res)] <- NA
            } else {
                warning("In swDynamicHeight() : returning NA since < 4 levels", call.=FALSE)
                res <- NA
            }
        }
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
        return(height(x, referencePressure, subdivisions=subdivisions, rel.tol=rel.tol, eos=eos))
    } else {
        stop("method is only for objects of class '", "section", " or '", "ctd", "'")
    }
}


#' Seawater lapse rate
#'
#' Compute adiabatic lapse rate
#'
#' If `eos="unesco"`, the density is calculated using the UNESCO equation
#' of state for seawater (references 1 and 2), and if `eos="gsw"`, the GSW formulation
#' (references 3 and 4) is used.
#'
#' @param salinity either salinity (PSU) (in which case `temperature` and
#' `pressure` must be provided) *or* a `ctd` object (in which
#' case `salinity`, `temperature` and `pressure` are determined
#' from the object, and must not be provided in the argument list).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` (references 1 and 2)
#' or `"gsw"` (references 3 and 4).
#'
#' @return Lapse rate (\eqn{deg}{deg}C/m).
#'
#' @author Dan Kelley
#'
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater.
#' *Unesco Technical Papers in Marine Science*,
#' *44*, 53 pp.  (Section 7, pages 38-40)
#'
#' @examples
#' lr <- swLapseRate(40, 40, 10000) # 3.255976e-4
#'
#' @family functions that calculate seawater properties
swLapseRate <- function(salinity, temperature=NULL, pressure=NULL,
                        longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    if (is.null(l$temperature))
        stop("must provide temperature")
    nt <- length(l$temperature)
    if (is.null(l$pressure)) l$pressure <- rep(0, length.out=nS)
    if (length(l$pressure) == 1)
        l$pressure <- rep(l$pressure[1], length.out=nS)
    np <- length(l$pressure)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        res <- .C("sw_lapserate", as.integer(nS), as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                   value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        ## the 1e4 is to convert from 1/Pa to 1/dbar
        res<- 1e4 * gsw::gsw_adiabatic_lapse_rate_from_CT(SA=SA, CT=CT, p=l$pressure)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}                                      # swLapseRate


#' Seawater density
#'
#' Compute \eqn{\rho}{rho}, the *in-situ* density of seawater.
#'
#' If `eos="unesco"`, the density is calculated using the UNESCO equation
#' of state for seawater (references 1 and 2), and if `eos="gsw"`, the GSW formulation
#' (references 3 and 4) is used.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object, in
#' which case `salinity`, `temperature` (in the ITS-90 scale; see
#' next item), etc. are inferred from the object.
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale.  This scale is used by GSW-style calculation (as
#' requested by setting `eos="gsw"`), and is the value contained within
#' `ctd` objects (and probably most other objects created with data
#' acquired in the past decade or two). Since the UNESCO-style calculation is
#' based on IPTS-68, the temperature is converted within the present function,
#' using [T68fromT90()].
#'
#' @param pressure pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` (references 1 and 2)
#' or `"gsw"` (references 3 and 4).
#'
#' @return *In-situ* density (kg/m\eqn{^3}{^3}).
#'
#' @section Temperature units: The UNESCO formulae are defined in terms of
#' temperature measured on the IPTS-68 scale, whereas the replacement GSW
#' formulae are based on the ITS-90 scale. Prior to the addition of GSW
#' capabilities, the various `sw*` functions took temperature to be in
#' IPTS-68 units. As GSW capabilities were added in early 2015, the assumed
#' unit of `temperature` was taken to be ITS-90.  This change means that
#' old code has to be modified, by replacing e.g. `swRho(S, T, p)` with
#' `swRho(S, T90fromT68(T), p)`. At typical oceanic values, the difference
#' between the two scales is a few millidegrees.
#'
#' @author Dan Kelley
#'
#' @seealso Related density routines include [swSigma0()] (and
#' equivalents at other pressure horizons), [swSigmaT()], and
#' [swSigmaTheta()].
#'
#' @references
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater.
#' *Unesco Technical Papers in Marine Science*,
#' *44*, 53 pp
#'
#' 2. Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
#' York, 662 pp.
#'
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 4. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#'
#' @examples
#' library(oce)
#' # The numbers in the comments are the check values listed in reference 1;
#' # note that temperature in that reference was on the T68 scale, but that
#' # the present function works with the ITS-90 scale, so a conversion
#' # is required.
#' swRho(35, T90fromT68(5),      0, eos="unesco") # 1027.67547
#' swRho(35, T90fromT68(5),  10000, eos="unesco") # 1069.48914
#' swRho(35, T90fromT68(25),     0, eos="unesco") # 1023.34306
#' swRho(35, T90fromT68(25), 10000, eos="unesco") # 1062.53817
#'
#' @family functions that calculate seawater properties
swRho <- function(salinity, temperature=NULL, pressure=NULL,
                  longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else { # must be "unesco"
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    if (is.null(l$temperature))
        stop("must provide temperature")
    if (is.null(l$pressure))
        stop("must provide pressure")
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
                   value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_rho(SA, CT, p=l$pressure)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater density anomaly
#'
#' Compute \eqn{\sigma_\theta}{sigma}, the density of seawater, minus 1000
#' kg/m\eqn{^3}{^3}.
#'
#' @inheritParams swRho
#'
#' @return Density anomaly (kg/m\eqn{^3}{^3}), as computed with [swRho()], minus-
#' 1000 kg/m\eqn{^3}{^3}.
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @examples
#' library(oce)
#' swSigma(35, 13, 1000, longitude=300, latitude=30, eos="gsw") # 30.82374
#' swSigma(35, T90fromT68(13), 1000, eos="unesco")    # 30.8183
#'
#' @family functions that calculate seawater properties
swSigma <- function(salinity, temperature=NULL, pressure=NULL,
                    longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    swRho(salinity, temperature, pressure,
          longitude=longitude, latitude=latitude, eos=eos) - 1000
}


#' Seawater quasi-potential density anomaly
#'
#' Compute \eqn{\sigma_t}{sigma-t}, a rough estimate of potential density of
#' seawater, minus 1000 kg/m\eqn{^3}{^3}.
#'
#' If the first argument is an [oce-class] object, then salinity, etc., are
#' extracted from it, and used for the calculation.
#'
#' @inheritParams swRho
#'
#' @return Quasi-potential density anomaly (kg/m\eqn{^3}{^3}), defined as the
#' density calculated with pressure set to zero.
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @examples
#' swSigmaT(35, 13, 1000, longitude=300, latitude=30, eos="gsw") # 26.39623
#' swSigmaT(35, T90fromT68(13), 1000, eos="unesco") # 26.39354
#'
#' @family functions that calculate seawater properties
swSigmaT <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
        swRho(l$salinity, l$temperature, pressure=rep(0, length(l$salinity)),
              longitude=l$longitude, latitude=l$latitude, eos=l$eos) - 1000
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
        swRho(l$salinity, l$temperature, pressure=rep(0, length(l$salinity)),
              eos=l$eos) - 1000
    }
}


#' Seawater potential density anomaly
#'
#' Compute the potential density (minus 1000 kg/m^3) that seawater would have if raised
#' adiabatically to the surface.  In the UNESCO system, this quantity is
#' is denoted \eqn{\sigma_\theta}{sigma-theta} (hence the function name), but in
#' the GSW system, it is denoted `sigma0`.
#'
#' If the first argument is an `oce` object, then salinity, etc., are
#' extracted from it, and used for the calculation instead of any values
#' provided in the other arguments.
#'
#' @inheritParams swRho
#'
#' @param referencePressure The reference pressure, in dbar.
#'
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}), defined as
#' \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
#' - 1000 kg/m\eqn{^3}{^3}.
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @examples
#' expect_equal(26.4212790994, swSigmaTheta(35, 13, 1000, eos="unesco"))
#'
#' @family functions that calculate seawater properties
swSigmaTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
                         longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
       stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        temperature <- salinity[["temperature"]]
        pressure <- salinity[["pressure"]]
        if (is.null(longitude))
            longitude <- salinity[["longitude"]]
        if (is.null(latitude))
            latitude <- salinity[["latitude"]]
        salinity <- salinity[["salinity"]]
    }
    if (eos == "gsw") {
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
    }
    if (is.null(temperature))
        stop("must provide temperature")
    if (is.null(pressure))
        stop("must provide pressure")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    np <- length(pressure)
    if (np == 1)
        pressure <- rep(pressure, length.out=nS)
    np <- length(pressure)
    if (nS != np)
        stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    referencePressure <- rep(referencePressure[1], length.out=nS)
    if (eos == "unesco") {
        theta <- swTheta(salinity=salinity, temperature=temperature, pressure=pressure,
                         referencePressure=referencePressure,
                         longitude=longitude, latitude=latitude, eos=eos)
        res <- swRho(salinity=salinity, temperature=theta, pressure=referencePressure,
                     longitude=longitude, latitude=latitude, eos=eos) - 1000
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=salinity, p=pressure, longitude=longitude, latitude=latitude)
        res <- gsw::gsw_pot_rho_t_exact(SA=SA, t=temperature, p=pressure, p_ref=referencePressure) - 1000
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
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
#'
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}).
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @family functions that calculate seawater properties
swSigma0 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        temperature <- salinity[["temperature"]]
        pressure <- salinity[["pressure"]]
        if (eos == "gsw") {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        salinity <- salinity[["salinity"]]
    }
    if (eos == "gsw") {
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
    }
    swSigmaTheta(salinity=salinity, temperature=temperature, pressure=pressure, referencePressure=0,
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
#'
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}).
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @family functions that calculate seawater properties
swSigma1 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity=salinity, temperature=temperature, pressure=pressure, referencePressure=1000,
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
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}).
#' @author Dan Kelley
#' @references See citations provided in the [swRho()] documentation.
#' @family functions that calculate seawater properties
swSigma2 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity=salinity, temperature=temperature, pressure=pressure, referencePressure=2000,
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
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}).
#' @author Dan Kelley
#' @references See citations provided in the [swRho()] documentation.
#' @family functions that calculate seawater properties
swSigma3 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity=salinity, temperature=temperature, pressure=pressure, referencePressure=3000,
                 longitude=longitude, latitude=latitude, eos=eos)
}

#' Seawater potential density anomaly referenced to 4000db pressure
#'
#' Compute \eqn{\sigma_\theta}{sigma}, the potential density of seawater (minus
#' 1000 kg/m\eqn{^3}{^3}), referenced to 4000db pressures.
#' This is defined as
#' \eqn{\sigma_1=\sigma_\theta=\rho(S,\theta(S,t,p),4000}{sigma_1=sigma_theta=rho(S,theta(S,t,p),4000)}
#' - 1000 kg/m\eqn{^3}{^3}.
#'
#' @inheritParams swRho
#'
#' @return Potential density anomaly (kg/m\eqn{^3}{^3}).
#'
#' @author Dan Kelley
#'
#' @references See citations provided in the [swRho()] documentation.
#'
#' @family functions that calculate seawater properties
swSigma4 <- function(salinity, temperature=NULL, pressure=NULL,
                     longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    swSigmaTheta(salinity=salinity, temperature=temperature, pressure=pressure, referencePressure=4000,
                 longitude=longitude, latitude=latitude, eos=eos)
}


#' Seawater sound absorption in dB/m
#'
#' Compute the sound absorption of seawater, in dB/m
#'
#' Salinity and pH are ignored in this formulation.  Several formulae may be
#' found in the literature, and they give results differing by 10 percent, as
#' shown in reference 3 for example.  For this reason, it is likely that more
#' formulations will be added to this function, and entirely possible that the
#' default may change.
#'
#' @inheritParams swRho
#'
#' @param frequency The frequency of sound, in Hz.
#'
#' @param formulation character string indicating the formulation to use, either
#' of `"fischer-simmons"` or `"francois-garrison"`; see \dQuote{References}.
#'
#' @param pH seawater pH
#'
#' @return Sound absorption in dB/m.
#'
#' @author Dan Kelley
#'
#' @references
#' 1. F. H. Fisher and V. P. Simmons, 1977.  Sound absorption in
#' sea water.  Journal of the Acoustical Society of America, 62(3), 558-564.
#'
#' 2. R. E. Francois and G. R. Garrison, 1982.  Sound absorption based on
#' ocean measurements.  Part II: Boric acid contribution and equation for total
#' absorption.  Journal of the Acoustical Society of America, 72(6):1879-1890.
#'
#' 3. \url{http://resource.npl.co.uk/acoustics/techguides/seaabsorption/}
#'
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
        f1 <- 1.32e3 * (T + 273.1) * exp(-1700 / (T + 273.1))           # (6)
        A2 <- 5.62e-8 + 7.52e-10 * T                                    # (7)
        f2 <- 1.55e7 * (T + 273.1)*exp(-3052 / (T + 273.1))             # (8)
        P2 <- 1 - 10.3e-4 * p + 3.7e-7 * p^2                            # (9)
        A3 <- (55.9 - 2.37 * T + 4.77e-2 * T^2  - 3.48e-4*T^3) * 1e-15  # (10)
        P3 <- 1 - 3.84e-4 * p + 7.57e-8 * p^2                           # (11)
        alpha <- (A1*f1*f^2) / (f1^2 + f^2) + (A2*P2*f2*f^2) / (f2^2 + f^2) + A3*P3*f^2 # (3a)
        alpha <- alpha * 8686 / 1000   # dB/m
    } else if (formulation == "francois-garrison") {
        S <- salinity
        T <- T68fromT90(temperature)
        D <- pressure # FIXME: approximation
        c <- 1412 + 3.21 * T + 1.19 * S + 0.0167 * D # sound speed m/s
        f <- frequency / 1000          # convert to kHz
        theta <- 273 + T
        ## f1 in kHz
        f1 <- 2.8 * sqrt(S / 35) * 10^(4 - 1245 / theta) # nolint (space before left parenthesis)
        ## subscript 1 for boric acid contribution
        ## A1 in dB / km / kHz
        A1 <- 8.86 / c * 10^(0.78 * pH - 5) # nolint (space before left parenthesis)
        P1 <- 1
        ## MgSO4 contribution
        A2 <- 21.44 * (S / c) * (1 + 0.025 * T) # dB / km / kHz
        P2 <- 1 - 1.37e-4 * D + 6.2e-9 * D^2
        ## f2 in kHz
        f2 <- (8.17 * 10^(8 - 1990 / theta)) / (1 + 0.0018 * (S - 35)) # nolint (space before left parenthesis)
        ## pure water contribution
        A3 <- 3.964e-4 - 1.146e-5 * T + 1.45e-7 * T^2 - 6.5e-10 * T^3 # dB / km / kHz^2
        P3 <- 1 - 3.83e-5 * D + 4.9e-10 * D^2
        alpha <- (A1 * P1 * f1 * f^2) / (f^2 + f1^2) + (A2 * P2 * f2 * f^2) / (f^2 + f2^2) + A3 * P3 * f^2
        alpha <- alpha / 1000
    }
    alpha
}


#' Seawater sound speed
#'
#' Compute the seawater speed of sound.
#'
#' If `eos="unesco"`, the sound speed is calculated using the formulation
#' in section 9 of Fofonoff and Millard (1983). If `eos="gsw"`, then the
#' [gsw::gsw_sound_speed()] function from the
#' \CRANpkg{gsw} package is used.
#'
#' @inheritParams swRho
#'
#' @return Sound speed (m/s).
#'
#' @author Dan Kelley
#'
#' @references Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
#' computation of fundamental properties of seawater.
#' *Unesco Technical Papers in Marine Science*,
#' *44*, 53 pp.  (See section 9.)
#'
#' @examples
#' swSoundSpeed(40, T90fromT68(40), 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
#'
#' @family functions that calculate seawater properties
swSoundSpeed <- function(salinity, temperature=NULL, pressure=NULL,
                         longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    dim <- dim(l$salinity)
    if (is.null(l$temperature))
        stop("must provide temperature")
    if (is.null(l$pressure))
        stop("must provide pressure")
    nS <- length(l$salinity)
    nt <- length(l$temperature)
    ##np <- length(l$pressure)
    if (nS != nt)
        stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    l$pressure <- rep(l$pressure, length.out=nS)
    if (eos == "unesco") {
        res <- .C("sw_svel", as.integer(nS), as.double(l$salinity), as.double(T68fromT90(l$temperature)), as.double(l$pressure),
                   value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=l$salinity, p=l$pressure, longitude=l$longitude, latitude=l$latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=l$temperature, p=l$pressure)
        res <- gsw::gsw_sound_speed(SA=SA, CT=CT, p=l$pressure)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}



#' Seawater specific heat
##' Source= http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f
##' check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
#'
#' Compute specific heat of seawater.
#'
#' If the first argument is a `ctd` object, then salinity, etc, are
#' extracted from it, and used for the calculation.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale.
#' @param pressure seawater pressure (dbar)
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#' @param eos equation of state, either `"unesco"` or `"gsw"`.
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
                           longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (eos == "gsw") {
        if (inherits(salinity, "oce")) {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude, eos=eos))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure, eos=eos))
    }
    if (is.null(l$temperature))
        stop("must provide temperature")
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
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater spiciness
#'
#' Compute seawater "spice", also called "spiciness" (a variable orthogonal
#' to density in TS space), in either of two formulations, depending on
#' the value of the `eos` argument. If `eos="unesco"` then
#' Flament's (reference 1) formulation is used. If `eos="gsw"`
#' then the Gibbs SeaWater formulation for "spiciness0" is used
#' (see reference 2).
#'
#' If the first argument is a `ctd` object, then salinity, temperature and
#' pressure values are extracted from it, and used for the calculation. (For
#' the `eos="gsw"` case, longitude and latitude are also extracted, because
#' these are required for the formulation of spiciness0.
#'
#' Roughly speaking, seawater with a high spiciness is relatively warm and
#' salty compared with less spicy water. Another interpretation is that spice
#' is a variable measuring distance orthogonal to isopycnal lines on TS
#' diagrams (if the diagrams are scaled to make the isopycnals run at 45
#' degrees). Note that pressure, longitude and latitude are all
#' ignored in the Flament definition.
#'
#' @param salinity either salinity (PSU) (in which case `temperature` and
#' `pressure` must be provided) *or* a `ctd` object (in which
#' case `salinity`, `temperature` and `pressure` are determined
#' from the object, and must not be provided in the argument list).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C) on the
#' ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure Seawater pressure (dbar) (only used if `eos` is
#' `"gsw"`); see \sQuote{Details}..
#'
#' @param longitude longitude of observation (only used if `eos` is
#' `"gsw"`; see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos` is
#' `"gsw"`; see \sQuote{Details}).
#'
#' @param eos Character value specifying the equation of state,
#' either `"unesco"` (for the Flament formulation, although this
#' is not actually part of UNESCO)
#' or `"gsw"` for the Gibbs SeaWater formulation.
#'
#' @return Flament-formulated spice \eqn{kg/m^3} if `eos` is `"unesco"`
#' or surface-referenced GSW spiciness0 \eqn{kg/m^3} if `eos` is `"gsw"`,
#' the latter provided by [gsw::gsw_spiciness0()], and hence aimed
#' at application within the top half-kilometre of the ocean.
#'
#' @author Dan Kelley coded this, merely an interface to the code described
#' by references 1 and 2.
#'
#' @examples
#' ## Contrast the two formulations.
#' library(oce)
#' data(ctd)
#' p <- ctd[["pressure"]]
#' plot(swSpice(ctd, eos="unesco"), p,
#'      xlim=c(-2.7, -1.5), ylim=rev(range(p)),
#'      xlab="Spice", ylab="Pressure (dbar)")
#' points(swSpice(ctd, eos="gsw"), p,col=2)
#' mtext("black=unesco, red=gsw")
#'
#' @references
#' 1. Flament, P. \dQuote{A State Variable for Characterizing Water Masses and Their
#' Diffusive Stability: Spiciness.} Progress in Oceanography, Observations of the
#' 1997-98 El Nino along the West Coast of North America, 54, no. 1
#' (July 1, 2002):493-501.
#' \url{https://doi.org/10.1016/S0079-6611(02)00065-4}
#'
#' 2.McDougall, Trevor J., and Oliver A. Krzysik. \dQuote{Spiciness.}
#' Journal of Marine Research 73, no. 5 (September 1, 2015): 141-52.
#' \url{https://doi.org/10.1357/002224015816665589}
#'
#' @family functions that calculate seawater properties
swSpice <- function(salinity, temperature=NULL, pressure=NULL,
                    longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        temperature <- salinity[["temperature"]]
        pressure <- salinity[["pressure"]]
        if (is.null(longitude))
            longitude <- salinity[["longitude"]]
        if (is.null(latitude))
            latitude <- salinity[["latitude"]]
        salinity <- salinity[["salinity"]]
    }
    if (eos == "gsw") {
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
    }
    if (eos == "gsw" && is.null(pressure))
        stop("must provide pressure")
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    if (is.null(pressure)) pressure <- rep(0, nS)
    if (length(pressure) == 1) pressure <- rep(pressure, length.out=nS)
    np <- length(pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    if (eos == "unesco") {
        res <- .C("sw_spice", as.integer(nS), as.double(salinity),
                  as.double(T68fromT90(temperature)), as.double(pressure),
                  value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
    } else if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=salinity, p=pressure, longitude=longitude, latitude=latitude)
        CT <- gsw::gsw_CT_from_t(SA=SA, t=temperature, p=pressure)
        res <- gsw::gsw_spiciness0(SA, CT)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater potential temperature
#'
#' Compute the potential temperature of seawater, denoted \eqn{\theta}{theta}
#' in the UNESCO system, and `pt` in the GSW system.
#'
#' Different formulae are used depending on the equation of state. If `eos`
#' is `"unesco"`, the method of Fofonoff *et al.* (1983) is used
#' (see references 1 and 2).
#' Otherwise, `swTheta` uses [gsw::gsw_pt_from_t()] from
#' the \CRANpkg{gsw} package.
#'
#' If the first argument is a `ctd` or `section` object, then values
#' for salinity, etc., are extracted from it, and used for the calculation, and
#' the corresponding arguments to the present function are ignored.
#'
#' @param salinity either salinity (PSU) (in which case `temperature` and
#' `pressure` must be provided) *or* an `oce` object (in which
#' case `salinity`, etc. are inferred from the object).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()], and the examples below.
#'
#' @param pressure pressure (dbar)
#'
#' @param referencePressure reference pressure (dbar)
#'
#' @param longitude longitude of observation (only used if `eos="gsw"`;
#' see \sQuote{Details}).
#'
#' @param latitude latitude of observation (only used if `eos="gsw"`; see
#' \sQuote{Details}).
#'
#' @param eos equation of state, either `"unesco"` (references 1 and 2) or `"gsw"`
#' (references 3 and 4).
##'
#' @return Potential temperature (\eqn{^\circ}{deg}C) of seawater, referenced
#' to pressure `referencePressure`.
#'
#' @author Dan Kelley
#'
#' @references
#'
#' 1. Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
#' fundamental properties of seawater.
#' *Unesco Technical Papers in Marine Science*, *44*, 53 pp
#'
#' 2. Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
#' York, 662 pp.
#'
#' 3. IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
#' seawater-2010: Calculation and use of thermodynamic properties.  Technical
#' Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.
#'
#' 4. McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
#' the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
#' ISBN 978-0-646-55621-5.
#'
#' @examples
#' library(oce)
#' ## test value from Fofonoff et al., 1983
#' expect_equal(36.8818748026, swTheta(40, T90fromT68(40), 10000, 0, eos="unesco"))
#'
#' # Example from a cross-Atlantic section
#' data(section)
#' stn <- section[['station', 70]]
#' plotProfile(stn, 'theta', ylim=c(6000, 1000))
#' lines(stn[['temperature']], stn[['pressure']], lty=2)
#' legend("bottomright", lty=1:2,
#'        legend=c("potential", "in-situ"),
#'        bg='white', title="Station 70")
#'
#' @family functions that calculate seawater properties
swTheta <- function(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
                    longitude=NULL, latitude=NULL, eos=getOption("oceEOS", default="gsw"))
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        temperature <- salinity[["temperature"]]
        pressure <- salinity[["pressure"]]
        if (eos == "gsw") {
            if (is.null(longitude))
                longitude <- salinity[["longitude"]]
            if (is.null(latitude))
                latitude <- salinity[["latitude"]]
        }
        salinity <- salinity[["salinity"]]
    }
    if (eos == "gsw") {
        if (is.null(longitude))
            stop("must supply longitude")
        if (is.null(latitude))
            stop("must supply latitude")
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                             longitude=longitude, latitude=latitude))
    } else {
        l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure))
    }
    dim <- dim(salinity)
    nS <- length(salinity)
    nt <- length(temperature)
    if (nS != nt) stop("lengths of salinity and temperature must agree, but they are ", nS, " and ", nt, ", respectively")
    np <- length(pressure)
    if (np == 1)
        pressure <- rep(pressure, length.out=nS)
    np <- length(pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    referencePressure <- rep(referencePressure[1], length.out=nS)
    if (eos == "gsw") {
        SA <- gsw::gsw_SA_from_SP(SP=salinity, p=pressure, longitude=longitude, latitude=latitude)
        res <- gsw::gsw_pt_from_t(SA=SA, t=temperature, p=pressure, p_ref=referencePressure)
    } else if (eos == "unesco") {
        ## Note the conversion to the T68 scale, because that's the scale
        ## used by the UNESCO formula.
        res <- .C("theta_UNESCO_1983",
                  as.integer(nS),
                  as.double(salinity), as.double(T68fromT90(temperature)), as.double(pressure),
                  as.double(referencePressure),
                  value=double(nS), NAOK=TRUE, PACKAGE="oce")$value
        res <- T90fromT68(res)
    }
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater viscosity
#'
#' Compute viscosity of seawater, in \eqn{Pa\cdot s}{Pa*s}
#'
#' If the first argument is a `ctd` object, then salinity, temperature and
#' pressure values are extracted from it, and used for the calculation.
#'
#' The result is determined from a regression of the data provided in Table 87
#' of Dorsey (1940). The fit matches the table to within 0.2 percent at worst,
#' and with average absolute error of 0.07 percent. The maximum deviation from
#' the table is one unit in the last decimal place.
#'
#' No pressure dependence was reported by Dorsey (1940).
#'
#' @param salinity either salinity (PSU) (in which case `temperature` and
#' `pressure` must be provided) *or* a `ctd` object (in which
#' case `salinity`, `temperature` and `pressure` are determined
#' from the object, and must not be provided in the argument list).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()], and the examples below.
#'
#' @return Viscosity of seawater in \eqn{Pa\cdot s}{Pa*s}.  Divide by density
#' to get kinematic viscosity in \eqn{m^2/s}{m^2/s}.
#'
#' @author Dan Kelley
#'
#' @references N. Ernest Dorsey (1940),
#' *Properties of ordinary Water-substance*,
#' American Chemical Society Monograph Series.  Reinhold
#' Publishing.
#'
#' @examples
#' swViscosity(30, 10) # 0.001383779
#'
#' @family functions that calculate seawater properties
swViscosity <- function(salinity, temperature)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (missing(temperature))
        stop("must provide temperature")
    l <- lookWithin(list(salinity=salinity, temperature=temperature))
    0.001798525 + l$salinity * (2.634749e-06 - 7.088328e-10 *
           l$temperature^2 + l$salinity * (-4.702342e-09 + l$salinity *
           (5.32178e-11))) + l$temperature * (-6.293088e-05 +
           l$temperature * (1.716685e-06 + l$temperature * (-3.479273e-08
           + l$temperature * (+3.566255e-10))))
}


#' Seawater conservative temperature, in GSW formulation
#'
#' Compute seawater Conservative Temperature, according to the GSW/TEOS-10
#' formulation.
#'
#' If the first argument is an `oce` object, then values for salinity,
#' etc., are extracted from it, and used for the calculation, and the
#' corresponding arguments to the present function are ignored.
#'
#' The conservative temperature is calculated using the TEOS-10 function
#' [gsw::gsw_CT_from_t] from the \CRANpkg{gsw} package.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#'
#' @param temperature *in-situ* temperature (\eqn{^\circ}{deg}C), defined
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure pressure (dbar)
#'
#' @param longitude longitude of observation.
#'
#' @param latitude latitude of observation.
#'
#' @return Conservative temperature in degrees Celcius.
#'
#' @author Dan Kelley
#'
#' @seealso The related TEOS-10 quantity ``absolute salinity'' may be computed
#' with [swAbsoluteSalinity()].  For a ctd object, conservative
#' temperature may also be recovered by indexing as e.g.
#' \code{ctd[["conservativeTemperature"]]} or \code{ctd[["CT"]]}.
## NOTE: the markdown-Rd translator balks on the above if backticks are used
#'
#' @references McDougall, T.J. and P.M. Barker, 2011: Getting started with
#' TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
#' SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.
#'
#' @examples
#' swConservativeTemperature(35,10,1000,188,4) # 9.86883
#'
#' @family functions that calculate seawater properties
swConservativeTemperature <- function(salinity, temperature=NULL, pressure=NULL,
                                      longitude=NULL, latitude=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        if (is.null(longitude))
            longitude <- salinity[["longitude"]]
        if (is.null(latitude))
            latitude <- salinity[["latitude"]]
    }
    if (is.null(longitude))
        stop("must supply longitude")
    if (is.null(latitude))
        stop("must supply latitude")
    l <- lookWithin(list(salinity=salinity, temperature=temperature, pressure=pressure,
                         longitude=longitude, latitude=latitude))
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
    if (!is.null(dim))
        dim(res) <- dim
    res
}


#' Seawater absolute salinity, in GSW formulation
#'
#' Compute the seawater Absolute Salinity, according to the GSW/TEOS-10
#' formulation with [gsw::gsw_SA_from_SP()] in the \CRANpkg{gsw} package.
#' Typically, this is a fraction of a unit
#' higher than practical salinity as defined in the UNESCO formulae.
#'
#' @param salinity either practical salinity (in which case `temperature`
#' and `pressure` must be provided) *or* an `oce` object (in
#' which case `salinity`, etc. are inferred from the object).
#'
#' @param pressure pressure in dbar.
#'
#' @param longitude longitude of observation.
#'
#' @param latitude latitude of observation.
#'
#' @return Absolute Salinity in \eqn{g/kg}{g/kg}.
#'
#' @author Dan Kelley
#'
#' @seealso The related TEOS-10 quantity ``conservative temperature'' may be
#' computed with [swConservativeTemperature()].  For a ctd object,
#' absolute salinity may also be recovered by indexing as e.g.
#' \code{ctd[["absoluteSalinity"]]} or \code{ctd[["SA"]]}.
## NOTE: the markdown-Rd translator balks on the above if backticks are used
#'
#' @references McDougall, T.J. and P.M. Barker, 2011: Getting started with
#' TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
#' SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.
#'
#' @examples
#'\dontrun{
#' sa <- swAbsoluteSalinity(35.5, 300, 260, 16)
#' stopifnot(abs(35.671358392019094 - sa) < 00.000000000000010)
#'}
#'
#' @family functions that calculate seawater properties
swAbsoluteSalinity <- function(salinity, pressure=NULL, longitude=NULL, latitude=NULL)
{
    if (missing(salinity))
        stop("must provide salinity")
    if (inherits(salinity, "oce")) {
        if (is.null(longitude))
            longitude <- salinity[["longitude"]]
        if (is.null(latitude))
            latitude <- salinity[["latitude"]]
    }
    if (is.null(longitude))
        stop("must supply longitude")
    if (is.null(latitude))
        stop("must supply latitude")
    l <- lookWithin(list(salinity=salinity, pressure=pressure, longitude=longitude, latitude=latitude))
    dim <- dim(l$salinity)
    nS <- length(l$salinity)
    np <- length(l$pressure)
    if (nS != np) stop("lengths of salinity and pressure must agree, but they are ", nS, " and ", np, ", respectively")
    bad <- is.na(l$salinity) | is.na(l$pressure) | is.na(l$longitude) | is.na(l$latitude)
    good <- gsw::gsw_SA_from_SP(l$salinity[!bad], l$pressure[!bad], l$longitude[!bad], l$latitude[!bad])
    res <- rep(NA, nS)
    res[!bad] <- good
    if (!is.null(dim))
        dim(res) <- dim
    res
}
