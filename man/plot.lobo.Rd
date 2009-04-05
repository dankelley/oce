\name{plot.lobo}

\alias{plot.lobo}

\title{Plot lobo data}

\description{Plot a summary diagram for lobo data.}

\usage{\method{plot}{lobo}(x, close.screens=TRUE, \dots)}

\arguments{
  \item{x}{A \code{lobo} object, e.g. as read by \code{\link{read.lobo}}.}
  \item{close.screens}{boolean, indicating whether to this function
    should use \code{\link{oce.close.screen}} to close the drawing
    screens after plotting.  Set this to \code{FALSE} in order to
    leave the screens open, enabling you to use
    \code{\link{oce.screen}} (with a\code{new=FALSE}) to add new items
    to the panels (see cautions at \code{\link{oce.split.screen}}).
    Note that \code{\link{oce.close.screen}} should be called after
    doing any such additions.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a summary plot for a lobo dataset.}

\value{None.}

\seealso{
  \code{\link{summary.lobo}} summarizes the information, while
  \code{\link{read.lobo}} scans it from a file.
}

\examples{
\dontrun{
uri <- paste("http://loboviz.satlantic.com/cgi-bin/nph-data.cgi?",
 	"min_date=20070110&max_date=20091230",
 	"&x=date&",
 	"y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
 	"data_format=text",sep="")
l <- read.lobo(uri)
plot(d)
}
}
\references{\url{http://www.satlantic.com/default.asp?mn=1.15.27.139} and
\url{http://www.mbari.org/lobo/}
}

\author{Dan Kelley}

\keyword{misc}
