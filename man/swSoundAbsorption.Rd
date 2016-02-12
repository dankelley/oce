% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swSoundAbsorption}

\alias{swSoundAbsorption}

\title{Seawater sound absorption in dB/m}

\description{Compute the sound absorption of seawater, in dB/m}

\usage{swSoundAbsorption(frequency, salinity, temperature, pressure, pH=8,
    formulation=c("fisher-simmons", "francois-garrison"))}

\arguments{
  \item{frequency}{frequency of sound in Hz}
  \item{salinity}{salinity in PSU}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
    on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{pressure}{water pressure in dbar}
  \item{pH}{seawater pH}
  \item{formulation}{specify the formulation to use; see references}
}

\details{Salinity and pH are ignored in this formulation.  Several formulae may
    be found in the literature, and they give results differing by 10 percent,
    as shown at [3] for example.  For this reason, it is likely that more
    formulations will be added to this function, and entirely possible that the
    default may change.}

\value{Sound absorption in dB/m.}

\examples{
## Fisher & Simmons (1977 table IV) gives 0.52 dB/km for 35 PSU, 5 degC, 500 atm
## (4990 dbar of water)a and 10 kHz
alpha <- swSoundAbsorption(35, 4, 4990, 10e3)

## reproduce part of Fig 8 of Francois and Garrison (1982 Fig 8)
f <- 1e3 * 10^(seq(-1,3,0.1)) # in KHz
plot(f/1000, 1e3*swSoundAbsorption(f, 35, 10, 0, formulation='fr'),
     xlab=" Freq [kHz]", ylab=" dB/km", type='l', log='xy')
lines(f/1000, 1e3*swSoundAbsorption(f, 0, 10, 0, formulation='fr'), lty='dashed')
legend("topleft", lty=c("solid", "dashed"), legend=c("S=35", "S=0"))
}

\references{[1] F. H. Fisher and V. P. Simmons, 1977.  Sound absorption in sea
    water.  J. Acoust. Soc. Am., 62(3), 558-564.

    [2]  R. E. Francois and G. R. Garrison, 1982.  Sound absorption based on
    ocean measurements.  Part II: Boric acid contribution and equation for
    total absorption.  J. Acoust. Soc. Am.,  72(6):1879-1890.

    [3] \url{http://resource.npl.co.uk/acoustics/techguides/seaabsorption/}
}
\author{Dan Kelley}

\keyword{misc}
