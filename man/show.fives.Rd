\name{show.fives}

\alias{show.fives}


\title{Show summary-style fivenum stats}

\description{Show summary-style fivenum stats}

\usage{show.fives(x, indent="    ")}

\arguments{
  \item{x}{a list that contains an item named \code{fives}, which is a
    matrix, containing data as created with \code{\link{fivenum}},
    with colnames and rownames specified.  See \dQuote{Details}.}
  \item{indent}{a spacer put at the left of each line of the result}
}

\details{This is a helper function for various \code{summary} functions.
  The colnames are normally \code{c("Min.", "1st Qu.", "Median", "3rd
  Qu.", "Max.")}, and the rownames are the names of the variables being
  summarized.}
      
\value{A multi-line string, suitable for printing.}

\examples{
library(oce)
data(ptr)
s <- summary(ptr)
show.fives(s)
}

\author{Dan Kelley}

\keyword{misc}
