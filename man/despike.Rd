\name{despike}

\alias{despike}

\title{Remove spikes from a time series}

\description{Remove spikes from a time series}

\usage{despike(x, reference=c("median","smooth", "trim"), n=4, k=7, min, max,
        replace=c("reference", "NA"), skip)}

\arguments{

    \item{x}{a vector of (time-series) values, a list of vectors, a data frame,
        or an object that inherits from class \code{oce}.}

    \item{reference}{indication of the type of reference time series to be used
        in the detection of spikes; see \sQuote{Details}.}

    \item{n}{an indication of the limit to differences between \code{x} and
        the reference time series, used for \code{reference="median"} or
        \code{reference="smooth"}; see \sQuote{Details.}}

    \item{k}{length of running median used with \code{reference="median"}, and
        ignored for other values of \code{reference}.}

    \item{min}{minimum non-spike value of \code{x}, used with
        \code{reference="trim"}.}

    \item{max}{maximum non-spike value of \code{x}, used with
        \code{reference="trim"}.}
        
    \item{replace}{an indication of what to do with spike values, with
        \code{"reference"} indicating to replace them with the reference time
        series, and \code{"NA"} indicating to replace them with \code{NA}.}

    \item{skip}{optional vector naming columns to be skipped. This is ignored
        if \code{x} is a simple vector. Any items named in \code{skip} will be
        passed through to the return value without modification.  In some
        cases, \code{despike} will set up reasonable defaults for \code{skip},
        e.g. for a \code{ctd} object, \code{skip} will be set to
        \code{c("time", "scan", "pressure")} if it is not supplied as an
        argument.}
        
}

\details{The method identifies spikes with respect to a "reference"
    time-series, and replaces these spikes with the reference value, or with
    \code{NA} according to the value of \code{action}.

    For \code{reference="median"}, the first step is to linearly interpolate
    across any gaps, in which \code{x==NA}.  Then the reference time series is
    constructed using \code{\link{runmed}} as a running median of \code{k}
    elements.  Then, the standard deviation of the difference between \code{x}
    and the reference is calculated.  Any \code{x} values that differ from the
    reference by more than \code{n} times this standard deviation are
    considered to be spikes.  If \code{replace="reference"}, these \code{x}
    values are replaced with the reference series, and the resultant time
    series is returned.  If \code{replace="NA"}, the spikes are replaced with
    \code{NA} in the returned time series. 

    For \code{reference="smooth"}, the processing is the same as for
    \code{"median"}, except that \code{\link{smooth}} is used to calculate the
    reference time series.
    
    For \code{reference="trim"}, the reference time series is constructed by
    linear interpolation across any regions in which \code{x<min} or
    \code{x>max}.  In this case, the value of \code{n} is ignored, and the
    return value either uses the reference time series for spikes, or
    \code{NA}, according to the value of \code{replace}.

}

\value{A new vector in which spikes are replaced as described above.}


\examples{
n <- 50
x <- 1:n
y <- rnorm(n=n)
y[n/2] <- 10                    # 10 standard deviations
plot(x, y, type='l')
lines(x, despike(y), col='red')
lines(x, despike(y, reference="smooth"), col='darkgreen')
lines(x, despike(y, reference="trim", min=-3, max=3), col='blue')
legend("topright", lwd=1, col=c("black", "red", "darkgreen", "blue"),
       legend=c("raw", "median", "smooth", "trim"))

# add a spike to a CTD object
data(ctd)
plot(ctd)
T <- ctd[["temperature"]]
T[10] <- T[10] + 10
ctd[["temperature"]] <- T
CTD <- despike(ctd)
plot(CTD)
}

\author{Dan Kelley}

\keyword{misc}
