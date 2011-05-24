\name{showThrees}

\alias{showThrees}
\alias{threes}


\title{Show min, mean, and max values}

\description{Show min, mean, and max values}

\usage{showThrees(x, indent="    ")}

\arguments{
  \item{x}{a list that contains an item named \code{threes}, which is a
    matrix, containing data as created with \code{\link{fivenum}},
    with colnames and rownames specified.  See \dQuote{Details}.}
  \item{indent}{a spacer put at the left of each line of the result}
}

\details{This is a helper function for various \code{summary} functions.
  The colnames are \code{c("Min.", "Mean", "Max.")}, and the rownames are the
  names of the variables being summarized.}
      
\value{A multi-line string, suitable for printing.}

\examples{
library(oce)
data(ptr)
s <- summary(ptr)
showThrees(s)
}

\author{Dan Kelley}

\keyword{misc}
