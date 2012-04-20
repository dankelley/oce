\name{teos}

\alias{teos}

\title{Interface to TEOS-10 library}

\description{Interface to TEOS-10 library}

\usage{teos(name, a1, a2, a3, a4, a5, a6, lib='/usr/local/lib/libgswteos-10.so')}

\arguments{
  \item{name}{the name of a TEOS-10 function.  Note that the function names in
      the TEOS-10 documentation are in mixed case, but those in the actual
      TEOS-10 library are \emph{lower case}.  Thus, for example, the function
      to computer absolute salinity must be named as \code{"gsw_sa_from_sp"},
      not \code{"gsw_SA_from_SP"}, as the documentation specifies.}
  \item{a1}{the first argument to the TEOS-10 function}
  \item{a2}{the second argument to the TEOS-10 function}
  \item{a3}{the third argument to the TEOS-10 function}
  \item{a4}{the fourth argument to the TEOS-10 function}
  \item{a5}{the fifth argument to the TEOS-10 function, if there is one}
  \item{a6}{the sixth argument to the TEOS-10 function, if there is one}
  \item{lib}{the name of the shared library that provides TEOS-10.}
}

\details{This is a provisional support function, whose name and interface
    characteristics may change.  In particular, the present version of
    \code{teos} only permits 3 or 4 arguments to be provided.
    
    For \code{teos} to work, the user must download the C version of TEOS-10
    from the website listed in the references, then type \code{make install} at
    the command line, and finally copy the resultant \code{libgswteos-10.so}
    file to the directory \code{/usr/local/lib}.   For example, the steps
    listed below were used on the author's (OSX-based) machine.
\preformatted{
curl -OL http://www.teos-10.org/software/gsw_c_v3_0.zip
unzip gsw_c_v3_0.zip 
cd gsw-3.0
make library
sudo mv libgswteos-10.so /usr/local/lib
}
}

\value{Whatever the TEOS-10 function returns, with dimensions matching those of
    \code{a1}.}

\examples{
\dontrun{
## do not run this in checks, because it will fail unless TEOS-10 is installed
library(oce)
data(ctd)
sp <- ctd[["salinity"]]
temperature <- ctd[["temperature"]]
n <- length(sp)
p <- ctd[["pressure"]]
lat <- rep(ctd[["latitude"]], n)
lon <- rep(ctd[["longitude"]], n)
sa <- teos("gsw_sa_from_sp", sp, p, lon, lat)
par(mfrow=c(1,2))
plot(ctd, which="salinity")
lines(sa, p, col='red')
plot(ctd, which="density")
rhopot <- teos("gsw_pot_rho_t_exact", sa, temperature, p, rep(0,length(p)))
lines(rhopot - 1000, p, col='red')


## The following demonstrations are from test values from the TEOS-10 supplied
## program "gsw_check_functions.c".  (These, with more digits, are part of the
## test sequence that is run when Oce is built; see the tests/eos.R in the source
## code.)

# Absolute salinity is sa=35.671 g/kg, for practical salinity 35.5 PSU, pressure
# 300 dbar, long 300 degE, and lat 16 degN.
teos("gsw_sa_from_sp", 35.5, 300, 260, 16)

# Conservative temperature is ct=14.930, for abs sal=35.7 g/kg, in-situ temp 15
# degC, and press 300 dbar.
teos("gsw_ct_from_t", 35.7, 15, 300)

# Density is 1026.456 kg/m^3 for sa=35.7 g/kg, ct = 20 degC and p=300
teos("gsw_rho", 35.7, 20, 300)

}
}

\references{ [1] McDougall, T.J. and P.M. Barker, 2011: Getting started with
    TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
    SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.

    [2] The TEOS-10 library is provided at \url{www.teos-10.org}.
    
    [3] A programming interface to the matlab version of the TEOS library is
    provided at \url{http://www.teos-10.org/pubs/gsw/html/gsw_contents.html}.}

\author{Dan Kelley}

\keyword{misc}
