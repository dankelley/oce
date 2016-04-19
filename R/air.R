#' Wind dataset
#' 
#' Wind data inferred from Figure 5 of Koch et al. (1983), provided to
#' illustrate the \code{\link{interpBarnes}} function.
#' Columns \code{wind$x} and \code{wind$y} are location, while \code{wind$z} is
#' the wind speed, in m/s.
#' 
#' @name wind
#' @docType data
#'
#' @references S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
#' interactive Barnes objective map anlaysis scheme for use with satellite and
#' conventional data,'' \emph{J.  Climate Appl.  Met.}, vol 22, p. 1487-1503.
#' @family datasets provided with \code{oce}
NULL


#' Air density
#' 
#' Compute, \eqn{\rho}{rho}, the \emph{in-situ} density of air.
#' 
#' This will eventually be a proper equation of state, but for now it's just
#' returns something from wikipedia (i.e. not trustworthy), and not using
#' humidity.
#' 
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ}{deg}C]
#' @param pressure pressure in Pa (NOT kPa) -- ignored at present
#' @param humidity ignored at present
#' @return \emph{In-situ} air density [kg/m\eqn{^3}{^3}].
#' @author Dan Kelley
#' @references National Oceanographic and Atmospheric Agency, 1976.  U.S.
#' Standard Atmosphere, 1976.  NOAA-S/T 76-1562.  (Available as of 2010-09-30
#' at
#' \url{http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf}).
#' @keywords misc
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

