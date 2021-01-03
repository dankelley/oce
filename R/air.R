#' Wind dataset
#'
#' Wind data inferred from Figure 5 of Koch et al. (1983), provided to
#' illustrate the [interpBarnes()] function.
#' Columns `wind$x` and `wind$y` are location, while `wind$z` is
#' the wind speed, in m/s.
#'
#' @name wind
#'
#' @docType data
#'
#' @references S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
#' interactive Barnes objective map analysis scheme for use with satellite and
#' conventional data,'' *J.  Climate Appl.  Met.*, vol 22, p. 1487-1503.
#'
#' @family datasets provided with oce
NULL


#' Air density
#'
#' Compute \eqn{\rho}{rho}, the *in-situ* density of dry air.
#'
#' This will eventually be a proper equation of state, but for now it just
#' uses a dry-air formula posted on wikipedia (i.e. not trustworthy).
#'
#' @param temperature *in-situ* temperature, in \eqn{^\circ}{deg}C.
#'
#' @param pressure numeric value for pressure in Pa (*not* the kPa used in public
#' weather forecasts).
#'
#' @param humidity ignored at present
#'
#' @return *In-situ* dry-air density, in kg/m\eqn{^3}{^3}.
#'
#' @author Dan Kelley
#'
#' @references
#' 1. <https://en.wikipedia.org/wiki/Density_of_air>
#' 2. National Oceanographic and Atmospheric Agency, 1976.  U.S.
#' Standard Atmosphere, 1976.  NOAA-S/T 76-1562.  (A PDF of this document may
#' be available at
#' `http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf`
#' or
#' `http://www.dtic.mil/cgi-bin/GetTRDoc?Location=U2&doc=GetTRDoc.pdf&AD=ADA035728`
#' although neither link has proven to be reliable.)
#'
#' @examples
#' degC <- seq(0,30,length.out=100)
#' p <- seq(98,102,length.out=100) * 1e3
#' contour(x=degC, y=p, z=outer(degC,p,airRho), labcex=1)
airRho <- function(temperature, pressure, humidity)
{
    Tkelvin <- temperature + 273.15
    ## http://en.wikipedia.org/wiki/Density_of_air
    M <- 0.0289644                      # kg/mol
    R <- 287.058
    R <- 8.31447
    ##1.225
    M * pressure / R / Tkelvin
}
