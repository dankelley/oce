\name{oce.modify}
\alias{oce.modify}
\title{Edit an oce object}
\description{Edit an element of a oce object}
\usage{oce.modify(x, action, reason="", person="")}
\arguments{
\item{x}{a \code{oce} object.}
\item{action}{a character string specifying the modification to be
  done. Normally, this is an R statement that makes an assignment to a
  member of \code{x}; see Details.}
\item{reason}{the reason for doing this.}
\item{person}{the person who is processing the data.}
}

\details{This function provides the ability to modify any element of an
  \code{oce} object, in a self-documenting way.  The modification is
  done simply by \code{\link{eval}}uating the \code{\link{parse}}ed
  character string stored in \code{action}.  Importantly, the string
  must refer to the object as \code{x}, and not to the object as it is
  named in the calling context (see Examples).

  The advantage of using \code{oce.modify} over simply manipulating the
  object directly is that the function leaves a clean trail in the
  processing log.
}

\examples{
library(oce)
data(ctd)
ctd2 <- oce.modify(ctd, "x$data$pressure <- x$data$pressure - 1", "set surface pressure")
summary(ctd2)
}
\author{Dan Kelley}
\keyword{misc}
