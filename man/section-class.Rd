\name{section-class}

\docType{class}

\alias{section-class}
\alias{[[,section-method}
\alias{[[<-,section-method}
\alias{show,section-method}

\title{Class to store hydrographic section data}

\description{Class to store hydrographic section data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{

    \subsection{Extracting values}{Individual stations may be accessed as e.g.
        \code{section[["station", 1]]} for the first station (which returns a
        \code{\link{ctd-class}} object; see the example).  Aggregated values of
        the quantities measured at each level of the CTD profiles contained
        within the section may be accessed as e.g.
        \code{section[["salinity"]]}.  This works for any quantity whose name
        is present in the constituent profiles.    Since it is often useful to
        pair such quantities with locations, \code{section[["latitude"]]} and
        \code{section[["longitude"]]} return vectors with values repeated for
        each level in each CTD (see the \code{pairs()} call in the example
        section).  If just one latitude or longitude is desired per station,
        e.g. \code{section[["latitude", "bystation"]]} may be used.}
        
    \subsection{Assigning values}{FIXME: fill this in when the interface has
        decided upon}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(ctd)}) displays information about the object.}
}

\seealso{Sections can be read with \code{\link{read.section}} or created with
    \code{\link{read.section}} or created from CTD objects by using
    \code{\link{makeSection}} or by combining an existing section with a CTD
    object with the \code{+} operator.  
    
    Sections may be sorted with \code{\link{sectionSort}}, smoothed with
    \code{\link{sectionSmooth}}, and gridded with \code{\link{sectionGrid}}.
    Gridded sections may be plotted with \code{\link{plot.section}}.  
    
    Statistical summaries are provided by \code{\link{summary.section}}, while
    overviews are provided by \code{show.section}.  
    
    Two section datasets are provided in the package: \code{\link{a03}},
    containing data along WOCE line A03, and \code{\link{section}}, containing
    data in Halifax Harbour.}

\examples{
library(oce)
data(a03)
plot(a03[['station', 1]])
pairs(cbind(z=-a03[["pressure"]],T=a03[["temperature"]],S=a03[["salinity"]]))
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}

