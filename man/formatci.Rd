\name{formatci}
\alias{formatci}
\title{Format a confidence interval in parenthetic notation}
\description{Format a confidence interval in parenthetic notation}

\usage{formatci(ci, style=c("+-", "parentheses"))}

\arguments{
  \item{ci}{vector of length 2 or 3; see \dQuote{Details}.}
  \item{style}{string indicating notation to be used.}
}

\details{If \code{ci} contains 2 elements they are taken to be the lower
  and upper ends of the confidence interval and the central value is
  taken to be the mean of these values.  If \code{ci} contains 3
  elements, the first and third elements are taken as the range of the
  confidence interval, and the second element is taken as the central
  value.

  The \code{+-} notation is conventional, e.g. \code{12.0+/-0.5} indicates that
  the best estimate of value is 12.0, but that the confidence interval
  ranges from 11.5 to 12.5.  In the \code{parentheses} notation, this
  would be denoted \code{12.0(5)}.
}

\value{Character string with the estimate and its uncertainty, in
  plus/minus or parenthetic notation.}

\examples{
cat("10 +- 2 is:", formatci(c(8, 12)), "\n")
cat("10 +- 2 is:", formatci(c(8, 12), "parentheses"), "\n")
}

\author{Dan Kelley}

\references{A brief introduction to the parenthetic notation is provided
  at \url{http://en.wikipedia.org/wiki/Uncertainty}.  A very detailed
  treatment of uncertainty is given in \emph{Evaluation of measurement
  data - Guide to the expression of uncertainty in measurement (JCGM
  100:2008)}, available at
  \url{http://www.bipm.org/utils/common/documents/jcgm/JCGM_100_2008_E.pdf};
  see especially pages 25 and 26, for the notation.
}

\keyword{misc}
