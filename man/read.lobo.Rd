% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{read.lobo}

\alias{read.lobo}

\title{Read a lobo data file}

\description{Read a data file created by a LOBO instrument.}

\usage{read.lobo(file, cols=7, processingLog)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{cols}{number of columns in dataset.}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{This version of \code{read.lobo} is really quite crude, having been
  developed mainly for a ``predict the Spring bloom'' contest at Dalhousie
  University.  In particular, the function assumes that the data columns are
  exactly as specified in the Examples section; if you reorder the columns or
  add new ones, this function is unlikely to work correctly. Furthermore, it
  should be noted that the file format was inferred simply by downloading files;
  the supplier makes no claims that the format will be fixed in time.

  It is also worth noting that there is no \code{\link{read.oce}}
  equivalent to \code{read.lobo}, because the file format has no
  recognizable header.  }

\value{An object of \code{\link[base]{class}} \code{"lobo"}, which has the
    following slots:
  \item{\code{data}}{: a \code{\link{list}} containing the following vectors:
    \describe{
      \item{\code{time}}{the times of observation}
      \item{\code{u}}{one horizontal component of velocity (necessarily
	eastward) [m/s]}
      \item{\code{v}}{an orthogonal component of horizontal velocity [m/s]}
      \item{\code{salinity}}{the salinity [PSU]}
      \item{\code{temperature}}{the in-situ temperature [degC]}
      \item{\code{p}}{the pressure [dbar]}
      \item{\code{nitrate}}{the nitrate concentration [unit?]}
      \item{\code{fluorescence}}{[unit?]}
    }
  }
  \item{\code{metadata}}{: a list containing \code{header}, the header from the
    data file.}
  \item{\code{processingLog}}{: a processingLog of processing, in the standard
    \code{oce} format.}
}

\note{The \code{oce} author was unable to find a description of the data format,
  and so \code{read.lobo} makes some restrictive assumptions about the data
  format, e.g.  trying to find columns whose names contain the following strings
\code{"date"}, 
\code{"current across"}, 
\code{"current along"},
\code{"nitrate"},
\code{"fluorescence"},
\code{"salinity"},
\code{"temperature"}
and \code{"Air temperature"}.  The \code{data} slot in the return value will be filled in with these
data, or with vectors with \code{NA} values, for unfound columns.
}

\seealso{The documentation for \code{\link{lobo-class}} explains the structure
  of LOBO objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
uri <- paste("http://lobo.satlantic.com/cgi-bin/nph-data.cgi?",
  "min_date=20070220&max_date=20070305",
  "&x=date&",
  "y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
  "data_format=text",sep="")
lobo <- read.lobo(uri)
}
}

\source{The file was created as given in the example.}

\references{\url{http://lobo.satlantic.com} and
  \url{http://www.mbari.org/lobo/}}

\author{Dan Kelley}

\keyword{misc}
