\name{plot.argo}

\alias{plot.argo}
\alias{plot,argo,missing-method}
\alias{plot,argo-method}

\title{Plot argo data}

\description{Plot a summary diagram for argo data.}

\usage{\S4method{plot}{argo}(x, which = 1, level,
     coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                 "coastlineWorldFine", "none"),
     cex=1, pch=1, type='p', col, fill=FALSE,
     adorn=NULL,
     projection=NULL, parameters=NULL, orientation=NULL,
     mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
     tformat,
     debug=getOption("oceDebug"), \dots)
}

\arguments{
    \item{x}{A \code{argo} object, e.g. as read by \code{\link{read.argo}}.}

    \item{which}{list of desired plot types.
        \itemize{
            \item \code{which=1} or \code{which="trajectory"} gives a 
            plot of the argo trajectory, with the coastline, if one is provided.

            \item \code{which=2} or \code{"salinity ts"} gives a time series of
            salinity at the indicated level(s)

            \item \code{which=3} or \code{"temperature ts"} gives a time series
            of temperature at the indicated level(s)

            \item \code{which=4} or \code{"TS"} gives a TS diagram at the
            indicated level(s)

            \item \code{which=5} or \code{"salinity profile"} gives a salinity
            profile of all the data (with S and p trimmed to the 1 and 99
            percentiles)

            \item \code{which=6} or \code{"temperature profile"} gives a
            temperature profile (with T and p trimmed to the 1 and 99
            percentiles)


        }
    }

    \item{level}{level to plot, for e.g. \code{which=2} and higher.  May be an
        integer, in which case it refers to an index of depth (1 being the top)
        or it may be the string "all" which means to plot all data.}

    \item{coastline}{string giving the coastline to be used in an Argo-location
        map, or \code{"best"} to pick the one with highest resolution, or
        \code{"none"} to avoid drawing the coastline.}

    \item{cex}{size of plotting symbols to be used if \code{type='p'}.}

    \item{pch}{type of plotting symbols to be used if \code{type='p'}.}

    \item{type}{plot type, either \code{"l"} or \code{"p"}.}

    \item{col}{optional list of colours for plotting.}

    \item{fill}{either a logical, indicating whether to fill the land with
        light-gray, or a colour name.  Owing to problems with some projections,
        the default is not to fill.}

    \item{adorn}{list of expressions to be executed for the panels in
        turn, e.g. to adorn the plots.  If the number matches the number 
        of panels, then the strings are applied to the appropriate panels,
        as they are drawn from top-left to bottom-right.   If only a single
        expression is provided, it is used for all panels. (See
        \dQuote{Examples}.)}

    \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
        also for \code{par(mar)}, computed from this.  The default is
        tighter than the R default, in order to use more space for the data
        and less for the axes.}

    \item{projection, parameters, orientation}{parameters specifying map
        projection; see \code{\link{mapPlot}}.  If
        \code{projection="automatic"}, however, a projection is devised from
        the data, with \code{stereographic} if the mean latitude exceeds 70N
        and \code{mollweide} otherwise.}
 
    \item{mar}{value to be used with \code{\link{par}("mar")}.}
    \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
        plot types that call that function.  (See \code{\link{strptime}} for the
        format used.)}
    \item{debug}{debugging flag}
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a summary plot for an Argo dataset.}

\value{None.}

\seealso{The documentation for \code{\link{argo-class}} explains the
    structure of argo objects, and also outlines the other functions dealing
    with them.}

\examples{
library(oce)
data(argo)
plot(argo, which="trajectory")
}

\references{\url{http://www.argo.ucsd.edu/}}

\author{Dan Kelley}

\keyword{hplot}
