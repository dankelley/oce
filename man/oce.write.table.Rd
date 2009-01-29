\name{oce.write.table}

\alias{oce.write.table}

\title{Write the data portion of object to a file}

\description{Write the data portion of object to a file}

\usage{oce.write.table(x, file="", ...)}

\arguments{
  \item{x}{an \code{oce} object that contains a \code{data} table.}
  \item{file}{file name, as passed to \code{\link[utils]{write.table}}.
    Use \code{""} to get a listing in the terminal window.}
  \item{...}{optional arguments passed to
    \code{\link[utils]{write.table}}.}
}

\details{The output has a line containing the names of the columns in
  \code{x$data}, each enclosed in double quotes.  After that line are
  lines for the data themselves.  The default is to separate data items
  by a single space character, but this can be altered by using a
  \code{sep} argument in the \code{...} list (see
  \code{\link[utils]{write.table}}).

  This function is really no more than a thin wrapper around
  \code{\link[utils]{write.table}}, the only difference being that row
  names are ommitted here, making for a file format that is more
  conventional in Oceanography.}

\value{The value of \code{\link[utils]{write.table}} is returned.}

\seealso{\code{\link[utils]{write.table}}, which does the actual work.}

\author{Dan Kelley}

\keyword{misc}
