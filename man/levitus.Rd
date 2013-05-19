\name{levitus}

\docType{data}

\alias{levitus}

\title{Sea-surface temperature and salinity}

\description{Data from the 2009 version of the Levitus Ocean Atlas, in the form
    of a list containing \code{longitude}, \code{latitude}, and sea-surface
    salinity \code{SSS} and temperature \code{SST}.  Missing values, 1E30 in
    the data files, are here converted to \code{NA}.}

\examples{
library(oce)
data(levitus)
par(mfrow=c(2,1))
imagep(levitus$longitude,levitus$latitude,levitus$SST,col=oceColorsJet,zlim=c(-2,30))
imagep(levitus$longitude,levitus$latitude,levitus$SSS,col=oceColorsJet,zlim=c(20,40))
}

\usage{data(levitus)}

\source{NOAA data repository
    (\url{http://www.nodc.noaa.gov/OC5/WOA09/netcdf_data.html})}

\author{Subsetted and stored in an R list by Dan Kelley.}

\references{

Locarnini, R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, H. E. Garcia, O.
K. Baranova, M. M. Zweng, and D. R. Johnson, 2010. World Ocean Atlas 2009,
Volume 1: Temperature. S. Levitus, Ed. NOAA Atlas NESDIS 68, U.S. Government
Printing Office, Washington, D.C., 184 pp.
 
Antonov, J. I., D. Seidov, T. P. Boyer, R. A. Locarnini, A. V. Mishonov, H. E.
Garcia, O. K. Baranova, M. M. Zweng, and D. R. Johnson, 2010. World Ocean Atlas
2009, Volume 2: Salinity. S. Levitus, Ed. NOAA Atlas NESDIS 69, U.S. Government
Printing Office, Washington, D.C., 184 pp.

}

\keyword{datasets}
