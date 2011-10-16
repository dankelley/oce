\name{noce-class}

\docType{class}

\alias{noce-class}

\alias{noce}

\title{base class of oce package}

\description{This is the base class of all objects in the \code{oce} package.}

\section{Slots}{
    These slots are common to \emph{all} classes in the \code{oce} package:
    \describe{
        \item{\code{metadata}}{List containing metadata, such as file names,
            instrument configurations, etc.}

        \item{\code{data}}{List containing the actual data.}

        \item{\code{processingLog}}{List containing a time-stamped record of
            changes that have been made to the object, either automatically
            recorded by functions in the package, or added by the user with
            \code{\link{oceEdit}}.}

    }
}
\examples{
a <- new('noce')
str(a)
}
\author{Dan Kelley}
\keyword{classes}
\keyword{oce}

