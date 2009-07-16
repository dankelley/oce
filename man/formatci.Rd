\name{formatci}
\alias{formatci}
\title{Format a confidence interval in parenthetic notation}
\description{Format a confidence interval in parenthetic notation}

\usage{formatci(ci, style=c("+-", "parentheses"), model)}

\arguments{
  \item{ci}{optional vector of length 2 or 3.}
  \item{style}{string indicating notation to be used.}
  \item{model}{optional regression model, e.g. returned by
    \code{\link{lm}} or \code{\link{nls}}.}
}

\details{If a \code{model} is given, then \code{ci} is ignored, and a
  confidence interval is calculated using \code{\link{confint}} with
  \code{level} set to 0.6914619.  This \code{level} corresponds to a
  range of plus or minus one standard deviation, for the t distribution
  and a large number of degrees of freedom (since \code{qt(0.6914619,
    100000)} is 0.5).

  Alternatively, if \code{ci} is given and contains 2 elements, they are
  taken to be the lower and upper ends of the confidence interval and
  the central value is taken to be the mean of these values.  And if
  \code{ci} contains 3 elements, the first and third elements are taken
  as the range of the confidence interval, and the second element is
  taken as the central value.

  In the \code{+-} notation, e.g. \code{12.0+/-0.2} would indicate that
  the best estimate of value is 12.0, and that the confidence interval
  ranges from 11.8 to 12.2.

  In the \code{parentheses} notation, e.g. \code{12.0(1)} again
  indicates that the best estimate of value is 12.0, and that the
  confidence interval ranges from 11.5 to 12.5, i.e. \emph{twice} the
  number in parentheses.  \emph{Note that this convention is not always
    followed; some use the factor 2, others use 1.  For this reason, this
    code may change, perhaps even disallowing the parentheses style
    altogether.}

  In cases where the uncertainty exceeds the value, it is not clear how
  best to format things for the \code{parentheses} style, and so the
  \code{+-} style is used in such cases.
}

\value{If \code{ci} is given, the result is a character string with the
  estimate and its uncertainty, in plus/minus or parenthetic notation.
  If \code{model} is given, the result is a 1-column matrix holding
  character strings, with row names corresponding to the parameters of
  the model.}

\examples{
x <- seq(0, 1, length.out=300)
y <- rnorm(n=300, mean=1, sd=2) + rnorm(n=300, mean=1, sd=1) * x
m <- lm(y~x)
print(formatci(model=m))
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
