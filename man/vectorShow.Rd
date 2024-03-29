% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{vectorShow}
\alias{vectorShow}
\title{Show Some Values From a List, Vector or Matrix}
\usage{
vectorShow(
  v,
  msg = "",
  postscript = "",
  digits = 5L,
  n = 2L,
  showNA = FALSE,
  showNewline = TRUE
)
}
\arguments{
\item{v}{the item to be summarized.  If this is a list of a vector of named
items, then information is provided for each element. Otherwise, information
is provided for the first and last \code{n} values.}

\item{msg}{optional character value indicating a message to show,
introducing the vector.  If not provided, then
a message is created from \code{v}. If \code{msg} is a non-empty string,
then that string is pasted together with a colon (unless \code{msg} already
contains a colon), before pasting a summary of data values.}

\item{postscript}{optional character value indicating an optional message
to append at the end of the return value.}

\item{digits}{for numerical values of \code{v}, this is the number of digits
to use, in formatting the numbers with \code{\link[=format]{format()}}; otherwise,
\code{digits} is ignored.}

\item{n}{number of elements to show at start and end. If \code{n}
is negative, then all the elements are shown.}

\item{showNA}{logical value indicating whether to show the number
of \code{NA} values. This is done only if the output contains ellipses,
meaning that some values are skipped, because if all values are shown,
it will be perfectly obvious whether there are any \code{NA} values.}

\item{showNewline}{logical value indicating whether to put a newline
character at the end of the output string.  The default, TRUE, is
convenient for printing, but using FALSE makes more sense if
the result is to be used with, e.g. \code{\link[=mtext]{mtext()}}.}
}
\value{
A string ending in a newline character, suitable for
display with \code{\link[=cat]{cat()}} or \code{\link[=oceDebug]{oceDebug()}}.
}
\description{
This is similar to \code{\link[=str]{str()}}, but it shows data at the first and last of the
vector, which can be quite helpful in debugging.
}
\examples{
# List
limits <- list(low = 0, high = 1)
vectorShow(limits)

# Vector of named items
planktonCount <- c(phytoplankton = 100, zooplankton = 20)
vectorShow(planktonCount)

# Vector
vectorShow(pi)

# Matrix
vectorShow(volcano)

# Other arguments
knot2mps <- 0.5144444
vectorShow(knot2mps, postscript = "knots per m/s")
vectorShow("January", msg = "The first month is")

}
\author{
Dan Kelley
}
