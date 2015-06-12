\name{oce-class}

\docType{class}

\alias{oce-class}
\alias{[[,oce-method}
\alias{[[<-,oce-method}
\alias{show,oce-method}

\alias{oce}

\title{base class of oce package}

\description{This is class from which all objects in the \code{oce} package
    inherit.}

\section{Slots}{
    \describe{
        \item{\code{metadata}}{List containing metadata, such as file names,
            instrument configurations, etc.}

        \item{\code{data}}{List containing the actual data.}

        \item{\code{processingLog}}{List containing a time-stamped record of
            changes that have been made to the object, either automatically
            recorded by functions in the package, or added by the user with
            \code{\link{oce.edit}}.}
    }
}

\section{Methods}{

    All classes in the Oce package inherit from this base class, so they all
    possess the following methods.  Note that some classes extend the methods,
    and these extensions are discusssed in the detailed documentation files.
    For example, see \code{\link{section-class}} to see how latitudes of
    sections may be extracted either with one value per station, or with a
    value for each of the pressures within each of the stations.

    \describe{

        \item{[[}{Provides read-only access to an item in the object's
            \code{metadata} or \code{data}.  The named item is sought first in
            \code{metadata}, where an exact match to the name is required. If
            it is not present in \code{metadata}, then a partial-name match is
            sought in \code{data}.}

        \item{[[<-}{Alters the named item in the object's \code{metadata} or
            \code{data}, the former being examined before the latter.   For
            example, to add 0.005 to the practical salinity of a CTD object
            named \code{ctd}, one might write \code{ctd[["salinity"]] <- 0.005
                + ctd[["salinity"]]}.}

        \item{show}{Displays brief information about the object named as an
            argument.  For example, \code{show(ctd)} would provide such an
            overview for a CTD object as discussed above.}
    }
}

\seealso{
Information on the classes that derive from this base class are found at the following links:
\code{\link{adp-class}},
\code{\link{adv-class}},
\code{\link{cm-class}},
\code{\link{coastline-class}},
\code{\link{ctd-class}},
\code{\link{drifter-class}},
\code{\link{echosounder-class}},
\code{\link{lisst-class}},
\code{\link{lobo-class}},
\code{\link{met-class}},
\code{\link{rsk-class}},
\code{\link{sealevel-class}},
\code{\link{section-class}},
\code{\link{tidem-class}},
\code{\link{topo-class}}, and
\code{\link{windrose-class}}.
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}

