\name{runderiv}

\alias{runderiv}

\title{Calculate dy/dx with running regression}

\description{Calculate dy/dx with running regression}

\usage{runderiv(x, y, window=c("hanning", "boxcar"), L)}

\arguments{
    \item{x}{a vector holding the x coordinates of grid.}
    \item{y}{a vector holding the y coordinates of grid.}
    \item{window}{weighting within the window; see \sQuote{Details}.}
    \item{L}{width of running window, in x units.  If not provided, a reasonable
        default will be used}
}

\details{The derivative is calculated from the slope of a localized
    least-squares regression model y=y(x).  The localization is defined by the
    x difference from the point in question, with data at distance exceeding
    L/2 being ignored.  With a \code{boxcar} window, all data within the local
    domain are treated equally, while with a \code{hanning} window, a
    raised-cosine weighting function is used; the latter produces smoother
    derivatives, which can be useful for noisy data.}

\value{A vector of derivative values.}
    
\examples{
library(oce)
x <- 1:100
y <- 1 + 2 * x + x^2/50 + 200*sin(x/5)
L <- 4
calc <- runderiv(x, y, L=L)
theory <- 2 + 0.04 * x + 40*cos(x/5)
par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(x, y, type='l')
lines(x[1]+c(0, L), rep(mean(y), 2), col='blue', lwd=3)
plot(x, theory, type='l', ylab="dy/dx")
lines(x[1]+c(0, L), rep(mean(theory), 2), col='blue', lwd=3)
lines(x, calc, col='red')
legend("top", lwd=1, col=c("black","red"),
       legend=c("theory", "calc"))

# Case 2: square of buoyancy frequency
data(ctd)
par(mfrow=c(1,1))
plot(ctd, which="N2")
rho <- swRho(ctd)
z <- swZ(ctd)
N2 <- -9.8 / mean(rho) * runderiv(z, rho)
lines(N2, -z, col='blue')
legend("bottomright", lwd=2, bg="white",
       col=c("darkred", "blue"),
       legend=c("swN2()", "using runderiv()"))
}

\section{Development status}{Devised 2014-Feb-06 with a particular purpose in
    mind, this function may be somewhat unstable until perhaps the middle of
    Mar, 2014.}

\author{Dan Kelley}

\keyword{misc}

